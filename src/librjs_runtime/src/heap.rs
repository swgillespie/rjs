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
//! partitioned by ECMAScript types that must be allocated
//! on the heap: Strings and Objects. 
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

const DEFAULT_SEGMENT_SIZE : usize = 32;
const DEFAULT_ARENA_SIZE: usize = 2;

pub type ESString = String;
pub type Object = (); // TODO

impl ToHeapObject for StringPtr {
    fn to_heap_object(&self) -> Option<HeapObject> {
        None
    }
}

impl ToHeapObject for ObjectPtr {
    fn to_heap_object(&self) -> Option<HeapObject> {
        None
    }
}

/// An object that can be allocated on the heap. The current heap
/// allows for Strings and Objects to be allocated on the heap.
#[derive(Copy, Clone, PartialEq)]
pub enum HeapObject {
    String(StringPtr),
    Object(ObjectPtr)
}

impl HeapObject {
    /// Marks a HeapObject as visited by the garbage collector.
    fn mark(&mut self) {
        match *self {
            HeapObject::String(ref mut ptr) => ptr.mark(),
            HeapObject::Object(ref mut ptr) => ptr.mark()
        }
    }
}

impl Trace for HeapObject {
    /// Traces all pointers contained by a HeapObject.
    fn trace(&self) -> IntoIter<HeapObject> {
        match *self {
            HeapObject::String(_) => vec![].into_iter(),
            HeapObject::Object(_) => /* TODO */ vec![].into_iter()
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
    string_arena: Arena<ESString>,
    rooted_set: Vec<(HeapObject, usize)>,
    allocs_since_last_gc: usize
}

impl Heap {
    /// Creates a new Heap, with nothing allocated.
    pub fn new() -> Heap {
        Heap {
            object_arena: Arena::new(),
            string_arena: Arena::new(),
            rooted_set: vec![],
            allocs_since_last_gc: 0
        }
    }

    /// Allocates an object on the garbage-collected heap and returns
    /// a rooted pointer to it.
    /// ## GC Considerations
    /// This function may trigger a garbage collection.
    pub fn allocate_object(&mut self) -> RootedObjectPtr {
        debug!(target: "gc", "allocating an object");
        let alloc = self.object_arena.allocate();
        if let Some(ptr) = alloc {
            debug!(target: "gc", "fast path alloc successful");
            self.allocs_since_last_gc += 1;
            return RootedPtr::new(self, ptr);
        }

        if self.should_gc() {
            self.collect();
            self.allocs_since_last_gc = 0;
        } else {
            self.object_arena.add_segment();
        }

        let good_alloc = if let Some(alloc) = self.object_arena.allocate() {
            alloc
        } else {
            warn!(target: "gc", "having to expand the heap anyway after performing a gc");
            self.object_arena.add_segment();
            self.object_arena.allocate()
                .expect("allocation after heap expansion should always succeed")
        };

        self.allocs_since_last_gc += 1;
        RootedPtr::new(self, good_alloc)
    }

    /// Allocates a string on the garbage-collected heap and returns
    /// a rooted pointer to it.
    /// ## GC Considerations
    /// This function may trigger a garbage collection.
    pub fn allocate_string(&mut self) -> RootedStringPtr {
        let alloc = self.string_arena.allocate();
        if let Some(ptr) = alloc {
            self.allocs_since_last_gc += 1;
            return RootedPtr::new(self, ptr);
        }

        if self.should_gc() {
            self.collect();
        } else {
            self.string_arena.add_segment();
        }

        let good_alloc = if let Some(alloc) = self.string_arena.allocate() {
            alloc
        } else {
            warn!(target: "gc", "having to expand the heap anyway after performing a gc");
            self.string_arena.add_segment();
            self.string_arena.allocate()
                .expect("allocation after heap expansion should always succeed")
        };

        self.allocs_since_last_gc += 1;
        RootedPtr::new(self, good_alloc)
    }

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

    /// Marks a heap object as rooted.
    fn root(&mut self, value: HeapObject) {
        for &mut (ref rooted_element, ref mut count) in &mut self.rooted_set {
            // try to find whether or not this thing
            // is already rooted.
            if *rooted_element == value {
                *count += 1;
                return;
            }
        }

        // if it's not, stick it in the set.
        self.rooted_set.push((value, 1))
    }

    /// Unroots a heap object. If the root count reaches zero,
    /// the heap object is removed from the rooted set - otherwise
    /// the root count is decremented.
    fn unroot<T: ToHeapObject>(&mut self, ptr: &RootedPtr<T>) {
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
        let mut stack = vec![];
        for &(root, _) in &self.rooted_set {
            stack.push(root);
        }

        while let Some(mut item) = stack.pop() {
            item.mark();
            stack.extend(item.trace());
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
    segments: Vec<Segment<T>>
}

impl<T: Default> Arena<T> {
    /// Creates a new Arena with the default size.
    pub fn new() -> Arena<T> {
        Arena {
            segments: (0..DEFAULT_ARENA_SIZE)
                .map(|_| Segment::new())
                .collect()
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

        self.segments.retain(|s| !s.is_empty());
    }
}

/// A segment is a collection of potential allocations. Each segment
/// is composed of a series of slots, a free list, and a bit set
/// indicating which slots are marked in a garbage collection.
pub struct Segment<T> {
    slots: Vec<T>,
    free_list: Vec<usize>,
    marked_set: BitSet
}

impl<T: Default> Segment<T> {
    /// Creates a new segment, with every slot in the free list and every bit in the bitset
    /// set to true.
    fn new() -> Segment<T> {
        Segment {
            slots: (0..DEFAULT_SEGMENT_SIZE)
                .map(|_| Default::default())
                .collect(),
            free_list: (0..DEFAULT_SEGMENT_SIZE).collect(),
            marked_set: BitSet::from_bit_vec(BitVec::from_elem(DEFAULT_SEGMENT_SIZE, true))
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
        if self.is_empty() {
            return None;
        }

        let idx = self.free_list.pop().unwrap();
        Some(GcPtr::new(self, idx))
    }

    /// Returns true if this segment is empty, false otherwise.
    fn is_empty(&self) -> bool {
        self.free_list.len() == DEFAULT_SEGMENT_SIZE
    }
}

/// A smart pointer that represents a pointer to memory managed
/// by the garbage collector. A `GcPtr` is not rooted and is
/// liable to be reclaimed if a garbage collection occurs.
pub struct GcPtr<T> {
    segment: *mut Segment<T>,
    index: usize
}

impl<T> GcPtr<T> {
    /// Creates a new, unrooted GcPtr.
    fn new(segment: &mut Segment<T>, index: usize) -> GcPtr<T> {
        GcPtr {
            segment: segment as *mut _,
            index: index
        }
    }
}

impl<T> Copy for GcPtr<T> {}

impl<T> Clone for GcPtr<T> {
    fn clone(&self) -> GcPtr<T> {
        *self
    }
}


impl<T> GcPtr<T> {
    /// Marks this GcPtr as reachable during garbage collection.
    fn mark(&mut self) {
        assert!(!self.segment.is_null(), "segment can not be null");
        let segment = unsafe { &mut *self.segment };
        segment.marked_set.remove(&self.index);
    }
}

impl<T> Deref for GcPtr<T> {
    type Target = T;
    fn deref(&self) -> &T {
        assert!(!self.segment.is_null(), "segment can not be null");
        let segment = unsafe { &mut *self.segment };
        &segment.slots[self.index]
    }
}

impl<T> DerefMut for GcPtr<T> {
    fn deref_mut(&mut self) -> &mut T {
        assert!(!self.segment.is_null(), "segment can not be null");
        let segment = unsafe { &mut *self.segment };
        &mut segment.slots[self.index]
    }
}

impl<T: PartialEq> PartialEq for GcPtr<T> {
    fn eq(&self, other: &GcPtr<T>) -> bool {
        self.deref().eq(other)
    }
}

impl<T: Eq> Eq for GcPtr<T> {}

pub type StringPtr = GcPtr<ESString>;
pub type ObjectPtr = GcPtr<Object>;

/// A smart pointer type that represents a pointer
/// to garbage collected memory that is marked as rooted.
/// The underlying pointer is unrooted when RootedPtr is
/// destructed.
pub struct RootedPtr<T: ToHeapObject> {
    heap: *mut Heap,
    ptr: T
}

impl<T: ToHeapObject> RootedPtr<T> {
    /// Creates a new RootedPtr and registers it as marked
    /// with the heap.
    fn new(heap: &mut Heap, ptr: T) -> RootedPtr<T> {
        let mut ptr = RootedPtr {
            heap: heap as *mut _,
            ptr: ptr
        };
        ptr.root();
        ptr
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

impl<T: ToHeapObject> Drop for RootedPtr<T> {
    fn drop(&mut self) {
        self.unroot()
    }
}

pub type RootedStringPtr = RootedPtr<StringPtr>;
pub type RootedObjectPtr = RootedPtr<ObjectPtr>;
