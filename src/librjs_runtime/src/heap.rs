//! This module provides the garbage-collected heap that
//! contains any and all ECMAScript objects that are
//! allocated by the runtime.
//!
//! ## Overview
//! The ECMAScript standard does not require a "heap" in the
//! traditional sense, but it is very convenient for an implementation
//! to define a garbage-collected heap from which ECMAScript objects
//! are allocated from. This implementation is no different from
//! other ES implementations in that it provides a garbage-collected
//! heap upon which to allocate ES objects.
//!
//! This garbage collector is of the "mark-and-sweep" variety. From a
//! high level, the garbage collector walks over all "live" pointers
//! and "marks" them. After that is done, the collector walks over
//! everything that has been allocated on the heap and deallocates
//! everything that has not been marked, since it is not reachable
//! from the mutator (the running program).
//!
//! This garbage collector is not much more complicated than that
//! above high-level description. It partitions the heap into
//! fixed-size *Arenas*, which are composed of multiple fixed-size
//! *segments*, upon which data is allocated.
//!
//! ### Arenas
//! The managed heap is partitioned into multiple Arenas, each of
//! which is composed of multiple segments. Today, the heap is
//! partitioned by ECMAScript type - one for each type.
//!
//! Each arena maintains a list of segments. Whenever an arena
//! is asked to allocate (through the `allocate` method), it will
//! find a segment with space to allocate upon, and allocate on
//! that segment. If every segment is full, either a collection
//! will be triggered or another segment will be allocated, based
//! on internal heuristics. If a GC was triggered, there should
//! be space on a segment - the heuristic is geared to only perform
//! a GC if the collector thinks that space will be cleared. The
//! allocator then allocates on the new space, cleared either through
//! a GC or allocated from a new segment.
//!
//! ### Segments
//! A segment is a collection of locations that the allocator is
//! free to use for ECMAScript objects. Each segment has a fixed
//! size, which means that it is not always possible to allocate
//! on a segment. When a segment contains no objects, it is
//! deallocated by the arena that owns it.
//!
//! ### GC Smart Pointers
//! The result of an allocation on the managed heap is a `GcPtr<T>`
//! smart pointer that consists of several things: a pointer to the
//! segment from which it came and an index into the segment.
//! A `GcPtr<T>` is *unrooted*, which means that it is not considered
//! to be a part of the mutator's "rooted set". In order to be considered
//! a root by the collector, it must be used through the `RootedPtr<T>`
//! smart pointer. `RootedPtr<T>` consists of the `GcPtr<T>` that it
//! wraps and a pointer to the heap from which it came.
//!
//! The heap pointer is because rooting information is kept by the
//! entire heap, not individual arenas, since there can exist pointers
//! from one Arena to another Arena. While `GcPtr<T>` is marked
//! `Copy`, `RootedPtr<T>` is not `Copy`.
//!
//! Whenever a `RootedPtr<T>` is constructed from a `GcPtr<T>`,
//! an entry is made in the heap that keeps track of the pointer and
//! its "root count", the number of times this pointer has been rooted.
//! For any root count greater than 1, the collector considers the
//! pointer to be rooted. The `Drop` implementation of `RootedPtr<T>`
//! decrements this count by 1.
//!
//! ### Tracing the heap
//! This garbage collector is *precise*, which means that the collector
//! needs to be able to identify every pointer contained within a heap
//! allocation so that each pointer can be marked.
//!
//! The `Trace` trait must be implemented for all objects that will
//! be allocated on the heap. when visiting an object during the mark
//! phase of collection, the collector will invoke the `Trace`
//! implementation of that object to obtain a list of all pointers that
//! object contains, which it will traverse.
//!
//! ### Aliasing Mutable Memory
//! There is nothing Rust abhors more than aliased, mutable memory.
//! This poses a problem for our garbage collection scheme, since
//! `GcPtr<T>` implements Copy and also implements DerefMut. By
//! simply copying the pointer and dereferencing it twice, we obtain
//! two mutable references to the same memory slot, which is undefined behavior.
//!
//! In order to combat this problem, each heap slot is placed in a `RefCell`.
//! It is legal to alias a RefCell, as it enforces the borrow checker's rules at runtime.
//! If an attempt is made to mutably borrow a GC cell twice, the result
//! will be a panic.
//!
//! ### Acknowledgements
//! This GC is heavily inspired by Nick Fitzgerald (fitzgen)'s
//! GC for [Oxischeme](https://github.com/fitzgen/oxischeme),
//! a Scheme implementation in Rust.
//! Many ideas are borrowed from that implementation and I have to
//! thank Nick for the inspiration.

use bit_set::BitSet;
use bit_vec::BitVec;
use std::vec::IntoIter;
use std::marker::Copy;
use std::ops::{Deref, DerefMut, Drop};
use std::cmp::{PartialEq, Eq};
use std::default::Default;
use std::cell::RefCell;
use std::mem;

use values::{Object, Activation};

macro_rules! allocate {
    ($name:ident, $output:ty, $arena_name:ident) => {
        pub fn $name(&mut self) -> $output {
            let alloc = self.$arena_name.allocate();
            if let Some(ptr) = alloc {
                debug!(target: "gc", "fast path alloc successful");
                self.allocs_since_last_gc += 1;
                return RootedPtr::new(self, ptr);
            }

            if self.should_gc() {
                self.collect();
                self.allocs_since_last_gc = 0;
            } else {
                self.$arena_name.add_segment();
            }

            let good_alloc = if let Some(alloc) = self.$arena_name.allocate() {
                alloc
            } else {
                warn!(target: "gc", "having to expand the heap anyway after performing a gc");
                self.$arena_name.add_segment();
                self.$arena_name
                    .allocate()
                    .expect("allocation after heap expansion should always succeed")
            };

            self.allocs_since_last_gc += 1;
            RootedPtr::new(self, good_alloc)
        }
    }
}

const DEFAULT_SEGMENT_SIZE: usize = 32;
const DEFAULT_ARENA_SIZE: usize = 2;

impl ToHeapObject for StringPtr {
    fn to_heap_object(&self) -> Option<HeapObject> {
        Some(HeapObject::String(*self))
    }
}

impl ToHeapObject for ObjectPtr {
    fn to_heap_object(&self) -> Option<HeapObject> {
        Some(HeapObject::Object(*self))
    }
}

impl ToHeapObject for NumberPtr {
    fn to_heap_object(&self) -> Option<HeapObject> {
        Some(HeapObject::Number(*self))
    }
}

impl ToHeapObject for BooleanPtr {
    fn to_heap_object(&self) -> Option<HeapObject> {
        Some(HeapObject::Boolean(*self))
    }
}

impl ToHeapObject for ActivationPtr {
    fn to_heap_object(&self) -> Option<HeapObject> {
        Some(HeapObject::Activation(*self))
    }
}

/// An object that can be allocated on the heap. The current heap
/// allows for Strings and Objects to be allocated on the heap.
#[derive(Copy, Clone, PartialEq)]
pub enum HeapObject {
    Number(NumberPtr),
    Boolean(BooleanPtr),
    String(StringPtr),
    Object(ObjectPtr),
    Activation(ActivationPtr)
}

impl HeapObject {
    /// Marks a HeapObject as visited by the garbage collector.
    fn mark(&mut self) {
        match *self {
            HeapObject::String(ref mut ptr) => ptr.mark(),
            HeapObject::Object(ref mut ptr) => ptr.mark(),
            HeapObject::Boolean(ref mut ptr) => ptr.mark(),
            HeapObject::Number(ref mut ptr) => ptr.mark(),
            HeapObject::Activation(ref mut ptr) => ptr.mark()
        }
    }

    /// Returns whether or not this heap object can contain pointers.
    /// Used to avoid calling the `Trace` impl of HeapObject when
    /// the returned iterator will be empty.
    fn contains_pointers(&self) -> bool {
        match *self {
            HeapObject::Object(_) => true,
            HeapObject::Activation(_) => true,
            _ => false,
        }
    }
}

impl Trace for HeapObject {
    /// Traces all pointers contained by a HeapObject.
    fn trace(&self) -> IntoIter<HeapObject> {
        match *self {
            HeapObject::String(_) => vec![].into_iter(),
            HeapObject::Number(_) => vec![].into_iter(),
            HeapObject::Boolean(_) => vec![].into_iter(),
            HeapObject::Object(obj) => obj.borrow().trace(),
            HeapObject::Activation(act) => act.borrow().trace()
        }
    }
}

/// A trait for things that can be converted into HeapObjects.
pub trait ToHeapObject {
    fn to_heap_object(&self) -> Option<HeapObject>;
}

/// A trait that anything that can be allocated on the heap must
/// implement. This trait provides a mechanism for the collector
/// to discover all pointers that a given object contains, so that
/// all pointers can be traced during the mark phase of the GC.
pub trait Trace {
    fn trace(&self) -> IntoIter<HeapObject>;
}

/// A struct that encapsulates all of the allocation and heap
/// management that a ECMAScript interpreter needs. It separates
/// the allocation of objects and strings into separate arenas,
/// while keeping track of which pointers are rooted by both arenas.
pub struct Heap {
    object_arena: Arena<Object>,
    string_arena: Arena<String>,
    number_arena: Arena<f64>,
    boolean_arena: Arena<bool>,
    activation_arena: Arena<Activation>,
    rooted_set: Vec<(HeapObject, usize)>,
    allocs_since_last_gc: usize,
}

impl Heap {
    /// Creates a new Heap, with nothing allocated.
    pub fn new() -> Heap {
        Heap {
            object_arena: Arena::new(),
            string_arena: Arena::new(),
            number_arena: Arena::new(),
            boolean_arena: Arena::new(),
            activation_arena: Arena::new(),
            rooted_set: vec![],
            allocs_since_last_gc: 0,
        }
    }

    // TODO - This unfortunate bit of copy-pasta is due to the fact
    // that the Rust borrow checker does not permit a struct and
    // a field from that struct to be borrowed mutably at the same time.
    // Therefore, we cannot pass both the Heap and an Arena by mutable
    // reference at the same time, since that would be an aliasing violation
    // (a write to self.arena could be visible through the arena parameter pointer,
    // which is undefined behavior according to Rust)
    //
    // During development, this gets expanded by a macro, although this
    // isn't optimal since rustdoc does not consider the doc comments
    // supplied here to be attached to the item generated by the macro.

    /// Allocates an object on the garbage-collected heap and returns
    /// a rooted pointer to it.
    /// ## GC Considerations
    /// This function may trigger a garbage collection.
    allocate!(allocate_object, RootedObjectPtr, object_arena);

    /// Allocates a string on the garbage-collected heap and returns
    /// a rooted pointer to it.
    /// ## GC Considerations
    /// This function may trigger a garbage collection.
    allocate!(allocate_string, RootedStringPtr, string_arena);

    /// Allocates a number on the garbage-collected heap and returns
    /// a rooted pointer to it.
    /// ## GC Considerations
    /// This function may trigger a garbage collection.
    allocate!(allocate_number, RootedNumberPtr, number_arena);

    /// Allocates a boolean on the garbage-collected heap and returns
    /// a rooted pointer to it.    /// ## GC Considerations
    /// This function may trigger a garbage collection.
    allocate!(allocate_boolean, RootedBooleanPtr, boolean_arena);

    /// Allocates an activation on the garbage-collected heap and returns
    /// a rooted pointer to it.
    /// ## GC Considerations
    /// This function may trigger a garbage collection.
    allocate!(allocate_activation, RootedActivationPtr, activation_arena);

    /// Initiates a garbage collection.
    /// ## GC Considerations
    /// This function does trigger a garbage collection.
    pub fn collect(&mut self) {
        info!(target: "gc", "beginning collection");
        self.mark_phase();
        self.sweep_phase();
        info!(target: "gc", "collection complete");
        self.allocs_since_last_gc = 0;
    }

    /// Returns the number of strings currently allocated
    /// on the heap.
    pub fn number_of_string_allocations(&self) -> usize {
        self.string_arena.number_of_allocations()
    }

    /// Returns the number of objects currently allocated
    /// on the heap.
    pub fn number_of_object_allocations(&self) -> usize {
        self.object_arena.number_of_allocations()
    }

    /// Returns the number of numbers currently allocated
    /// on the heap.
    pub fn number_of_number_allocations(&self) -> usize {
        self.number_arena.number_of_allocations()
    }

    /// Returns the number of numbers currently allocated
    /// on the heap.
    pub fn number_of_boolean_allocations(&self) -> usize {
        self.boolean_arena.number_of_allocations()
    }

    /// Tries to determine whether or not a GC is a good
    /// idea based on advanced heuristics.
    fn should_gc(&self) -> bool {
        // TODO this is really lame. We can come up with a better
        // heuristic than this
        if self.allocs_since_last_gc < 20 {
            info!(target: "gc", "deciding to expand the heap because \
                                 only {} allocs have happened since \
                                 last GC", self.allocs_since_last_gc);
            false
        } else {
            info!(target: "gc", "deciding to GC because {} allocs \
                                 have occured since last GC", self.allocs_since_last_gc);
            true
        }
    }

    pub fn root_value<T: ToHeapObject>(&mut self, ptr: T) -> RootedPtr<T> {
        RootedPtr::new(self, ptr)
    }

    /// Marks a heap object as rooted.
    fn root(&mut self, value: HeapObject) {
        debug!(target: "gc", "rooting a pointer");
        for &mut (ref rooted_element, ref mut count) in &mut self.rooted_set {
            // try to find whether or not this thing
            // is already rooted.
            if *rooted_element == value {
                *count += 1;
                debug!(target: "gc", "target pointer was already rooted, \
                                      incrementing root count to {}", count);
                return;
            }
        }

        debug!(target: "gc", "pointer was not rooted, adding to rooted set");
        // if it's not, stick it in the set.
        self.rooted_set.push((value, 1))
    }

    /// Unroots a heap object. If the root count reaches zero,
    /// the heap object is removed from the rooted set - otherwise
    /// the root count is decremented.
    fn unroot<T: ToHeapObject>(&mut self, ptr: &RootedPtr<T>) {
        debug!(target: "gc", "unrooting a pointer");
        // apologies (and thanks) to fitzgen for this one, this is
        // the same basic strategy as his function that unroots a pointer.
        if let Some(heap_object) = ptr.to_heap_object() {
            let mut new_set = vec![];
            for &(root, count) in &self.rooted_set {
                if root == heap_object {
                    if count == 1 {
                        // skip putting this entry into
                        // the next set, since count == 0
                        // implies the object is unrooted
                        continue;
                    }

                    // otherwise, decrease the root count by one
                    new_set.push((root, count - 1));
                } else {
                    new_set.push((root, count));
                }
            }

            self.rooted_set = new_set;
        }
    }

    /// Mark phase of the garbage collector. Starting at the roots,
    /// mark_phase traverses the entire heap, marking pointers as it
    /// goes. At the end of the mark phase, all marked pointers are live
    /// and all unmarked pointers are dead.
    fn mark_phase(&mut self) {
        debug!(target: "gc", "begin mark phase");
        debug!(target: "gc", "there are {} roots", self.rooted_set.len());
        let mut worklist = vec![];
        for &(root, _) in &self.rooted_set {
            worklist.push(root);
        }

        while let Some(mut item) = worklist.pop() {
            item.mark();
            if item.contains_pointers() {
                worklist.extend(item.trace());
            }
        }
        debug!(target: "gc", "mark phase complete");
    }

    /// Collects the unmarked pointers identified in the mark phase
    /// into free lists, deallocating any segments that have become
    /// empty as a result of garbage collection.
    fn sweep_phase(&mut self) {
        debug!(target: "gc", "begin sweep phase for object arena");
        self.object_arena.sweep();
        debug!(target: "gc", "begin sweep phase for string arena");
        self.string_arena.sweep();
        debug!(target: "gc", "begin sweep phase for number arena");
        self.number_arena.sweep();
        debug!(target: "gc", "begin sweep phase for boolean arena");
        self.boolean_arena.sweep();
        debug!(target: "gc", "begin sweep phase for activation arena");
        self.activation_arena.sweep();
        debug!(target: "gc", "sweep phase complete");
    }
}

impl Default for Heap {
    fn default() -> Heap {
        Heap::new()
    }
}

/// An Arena consists of some number of segments. It's not always possible
/// to allocate on an Arena if every segment is full, in which case
/// additional segments must be added.
struct Arena<T> {
    segments: Vec<Segment<T>>,
}

impl<T: Default> Arena<T> {
    /// Creates a new Arena with the default size.
    pub fn new() -> Arena<T> {
        Arena {
            segments: (0..DEFAULT_ARENA_SIZE)
                          .map(|_| Segment::new())
                          .collect(),
        }
    }

    /// Allocates on a free segment and returns a pointer
    /// to the allocation, if there is free space on a
    /// segment. If there are no free spaces, thie method returns
    /// None.
    fn allocate(&mut self) -> Option<GcPtr<T>> {
        for segment in &mut self.segments {
            let allocation = segment.allocate();
            if allocation.is_some() {
                return allocation;
            }
        }

        None
    }

    /// Adds a new segment to this arena.
    fn add_segment(&mut self) {
        self.segments.push(Segment::new());
    }

    /// Sweeps every segment in this arena, placing unmarked
    /// allocations back into the free list for each segment.
    /// Any empty segments left over are deallocated.
    fn sweep(&mut self) {
        for segment in &mut self.segments {
            segment.sweep();
        }

        // TODO(perf) - This cleans out all empty segments, possibly
        // reducing the number of segments to zero if the heap is empty,
        // incurring the penalty of allocating a new segment on the next
        // allocation. We should probably try to leave at least one.
        self.segments.retain(|s| !s.is_empty());
    }

    fn number_of_allocations(&self) -> usize {
        self.segments
            .iter()
            .map(|s| s.number_of_allocations())
            .fold(0, |acc, item| acc + item)
    }
}

/// A segment is a collection of potential allocations. Each segment
/// is composed of a series of slots, a free list, and a bit set
/// indicating which slots are marked in a garbage collection.
struct Segment<T> {
    slots: Vec<RefCell<T>>,
    free_list: Vec<usize>,
    marked_set: BitSet,
}

impl<T: Default> Segment<T> {
    /// Creates a new segment, with every slot in the free list and every bit in the bitset
    /// set to true.
    fn new() -> Segment<T> {
        Segment {
            slots: (0..DEFAULT_SEGMENT_SIZE)
                       .map(|_| RefCell::new(Default::default()))
                       .collect(),
            free_list: (0..DEFAULT_SEGMENT_SIZE).collect(),
            marked_set: BitSet::from_bit_vec(BitVec::from_elem(DEFAULT_SEGMENT_SIZE, true)),
        }
    }

    /// Marking a set toggles a bit in the bit set from true to false. In the sweep phase,
    /// the only bits that should still be true correspond to the slots that were not
    /// marked. For every unmarked slot, add it to the free list.
    fn sweep(&mut self) {
        for idx in self.marked_set.iter() {
            if !self.free_list.contains(&idx) {
                self.free_list.push(idx);
            }
        }

        // reset the marked set for the next GC.
        self.marked_set = BitSet::from_bit_vec(BitVec::from_elem(DEFAULT_SEGMENT_SIZE, true))
    }

    /// Allocates from this segment if there is space available, returning None
    /// if there is not.
    fn allocate(&mut self) -> Option<GcPtr<T>> {
        if self.is_full() {
            return None;
        }

        let idx = self.free_list.pop().unwrap();
        Some(GcPtr::new(self, idx))
    }

    /// Returns true if this segment is empty, false otherwise.
    fn is_empty(&self) -> bool {
        self.free_list.len() == DEFAULT_SEGMENT_SIZE
    }

    fn is_full(&self) -> bool {
        self.free_list.len() == 0
    }

    fn number_of_allocations(&self) -> usize {
        DEFAULT_SEGMENT_SIZE - self.free_list.len()
    }
}

/// A smart pointer that represents a pointer to memory managed
/// by the garbage collector. A `GcPtr` is not rooted and is
/// liable to be reclaimed if a garbage collection occurs.
pub struct GcPtr<T> {
    segment: *mut Segment<T>,
    index: usize,
}

impl<T> GcPtr<T> {
    /// Creates a new, unrooted GcPtr.
    fn new(segment: &mut Segment<T>, index: usize) -> GcPtr<T> {
        GcPtr {
            segment: segment as *mut _,
            index: index,
        }
    }
}

impl<T> Copy for GcPtr<T> {}

impl<T> Clone for GcPtr<T> {
    fn clone(&self) -> GcPtr<T> {
        *self
    }
}

impl<T> PartialEq for GcPtr<T> {
    fn eq(&self, other: &GcPtr<T>) -> bool {
        // two GcPtrs are equal if they point to the same thing.
        // pointer equality in this scheme is defined by a pointer
        // pointing to the same slot on the same segment.
        self.segment == other.segment && self.index == other.index
    }
}

impl<T: Eq> Eq for GcPtr<T> {}

impl<T> GcPtr<T> {
    /// Marks this GcPtr as reachable during garbage collection.
    fn mark(&mut self) {
        assert!(!self.segment.is_null(), "segment can not be null");
        let segment = unsafe { &mut *self.segment };
        segment.marked_set.remove(&self.index);
    }
}

impl<T> Deref for GcPtr<T> {
    type Target = RefCell<T>;
    fn deref(&self) -> &RefCell<T> {
        assert!(!self.segment.is_null(), "segment can not be null");
        let segment = unsafe { &mut *self.segment };
        &segment.slots[self.index]
    }
}

impl<T> DerefMut for GcPtr<T> {
    fn deref_mut(&mut self) -> &mut RefCell<T> {
        assert!(!self.segment.is_null(), "segment can not be null");
        let segment = unsafe { &mut *self.segment };
        &mut segment.slots[self.index]
    }
}


pub type StringPtr = GcPtr<String>;
pub type ObjectPtr = GcPtr<Object>;
pub type NumberPtr = GcPtr<f64>;
pub type BooleanPtr = GcPtr<bool>;
pub type ActivationPtr = GcPtr<Activation>;

/// A smart pointer type that represents a pointer
/// to garbage collected memory that is marked as rooted.
/// The underlying pointer is unrooted when RootedPtr is
/// destructed.
pub struct RootedPtr<T: ToHeapObject> {
    heap: *mut Heap,
    ptr: T,
}

impl<T: ToHeapObject> RootedPtr<T> {
    /// Creates a new RootedPtr and registers it as marked
    /// with the heap.
    pub fn new(heap: &mut Heap, ptr: T) -> RootedPtr<T> {
        let mut ptr = RootedPtr {
            heap: heap as *mut _,
            ptr: ptr,
        };
        ptr.root();
        ptr
    }

    /// Swaps the pointer that this rooted pointer is rooting
    /// by unrooting the existing element, assigning the
    /// new element to the rooted pointer, and rooting the
    /// new value.
    pub fn swap(&mut self, other: T) {
        self.unroot();
        self.ptr = other;
        self.root();
    }

    /// Marks this pointer as rooted.
    fn root(&mut self) {
        if let Some(heap_obj) = self.to_heap_object() {
            assert!(!self.heap.is_null(), "heap can not be null");
            let heap = unsafe { &mut *self.heap };
            heap.root(heap_obj);
        }
    }

    /// Unroots this pointer.
    fn unroot(&mut self) {
        assert!(!self.heap.is_null(), "heap can not be null");
        let heap = unsafe { &mut *self.heap };
        heap.unroot(self);
    }
}

impl<T: ToHeapObject> Deref for RootedPtr<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.ptr
    }
}

impl<T: ToHeapObject> DerefMut for RootedPtr<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.ptr
    }
}

impl<T: ToHeapObject + Copy> Clone for RootedPtr<T> {
    fn clone(&self) -> RootedPtr<T> {
        assert!(!self.heap.is_null(), "heap can never be null");
        let heap = unsafe { &mut *self.heap };
        RootedPtr::new(heap, self.ptr)
    }
}

impl<T: ToHeapObject + Copy> RootedPtr<T> {
    /// Unroots this pointer and returns the unrooted pointer.
    /// Be very careful with this function! You cannot call
    /// ANY function that may trigger a GC while the returned
    /// pointer is still on the stack, as that could possibly
    /// invalidate the pointer.
    pub fn into_inner(mut self) -> T {
        self.unroot();
        let ptr = self.ptr;
        // don't run RootedPtr's destructor - it'll just unroot
        // the pointer and we already did that.
        mem::forget(self);
        ptr
    }
}

impl<T: ToHeapObject> Drop for RootedPtr<T> {
    fn drop(&mut self) {
        self.unroot()
    }
}

pub type RootedStringPtr = RootedPtr<StringPtr>;
pub type RootedObjectPtr = RootedPtr<ObjectPtr>;
pub type RootedNumberPtr = RootedPtr<NumberPtr>;
pub type RootedBooleanPtr = RootedPtr<BooleanPtr>;
pub type RootedActivationPtr = RootedPtr<ActivationPtr>;

#[cfg(test)]
mod tests {
    use super::*;
    extern crate env_logger;

    #[test]
    fn test_smoke_test() {
        let _ = env_logger::init();
        let mut heap = Heap::new();
        let alloc = heap.allocate_string();
        let value = alloc.borrow();
        assert_eq!(*value, String::new());
    }

    #[test]
    fn test_heap_is_writable() {
        let _ = env_logger::init();
        let mut heap = Heap::new();
        let alloc = heap.allocate_string();
        *alloc.borrow_mut() = "hello, world!".to_string();
        assert_eq!(&**alloc.borrow(), "hello, world!".to_string());
    }

    #[test]
    #[should_panic]
    fn test_soundness_hole_panic() {
        let _ = env_logger::init();
        let mut heap = Heap::new();
        let _alloc = heap.allocate_string();
        let copied_ptr = *_alloc;
        let other_copied_ptr = *_alloc;
        // copied_ptr and other_copied_ptr can be used to alias the
        // same cell of garbage-collected memory. However, it's
        // a RefCell, so trying to break the pointer aliasing rules
        // should panic.
        let mut writable = copied_ptr.borrow_mut();
        *writable = "hello world!".to_string();
        let _read = other_copied_ptr.borrow();
        // the above call should panic instead of allowing the
        // aliasing violation
    }

    #[test]
    fn test_pointer_rooting() {
        let _ = env_logger::init();
        let mut heap = Heap::new();
        let _alloc = heap.allocate_string();
        // alloc is currently rooted
        assert!(1 == heap.number_of_string_allocations(),
                "failed to allocate string on heap");
        heap.collect();
        // alloc should not be collected since it is rooted
        assert!(1 == heap.number_of_string_allocations(),
                "collected a rooted pointer");
    }

    #[test]
    fn test_pointer_collection() {
        let _ = env_logger::init();
        let mut heap = Heap::new();
        {
            let _alloc = heap.allocate_string();
            assert!(1 == heap.number_of_string_allocations(),
                    "failed to allocate string on heap");
            heap.collect();
            assert!(1 == heap.number_of_string_allocations(),
                    "collected a rooted pointer");
        }

        // alloc is out of scope and is now unrooted.
        // it should be reclaimed by the next collection
        heap.collect();
        assert!(0 == heap.number_of_string_allocations(),
                "failed to collect an unrooted pointer");
    }
}
