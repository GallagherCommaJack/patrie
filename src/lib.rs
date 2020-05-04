use slab::Slab;
use smallvec::SmallVec;
use std::num::NonZeroUsize;

pub struct Trie<T> {
    node_slab: Slab<Node>,
    item_slab: Slab<T>,
    root: Node,
}

mod keys {
    use super::*;
    #[derive(Clone, Copy, Debug)]
    pub(super) struct ItemKey(NonZeroUsize);

    impl ItemKey {
        fn as_usize(self) -> usize {
            self.0.get() - 1
        }

        pub(super) fn insert<T>(slab: &mut Slab<T>, val: T) -> Self {
            let raw = slab.insert(val);
            let key = unsafe { NonZeroUsize::new_unchecked(raw + 1) };
            Self(key)
        }

        pub(super) fn get<T>(self, slab: &Slab<T>) -> &T {
            slab.get(self.as_usize()).unwrap()
        }

        pub(super) fn get_mut<T>(self, slab: &mut Slab<T>) -> &mut T {
            slab.get_mut(self.as_usize()).unwrap()
        }

        pub(super) fn remove<T>(self, slab: &mut Slab<T>) -> T {
            slab.remove(self.as_usize())
        }
    }

    #[derive(Clone, Copy, Debug)]
    pub(super) struct NodeKey(NonZeroUsize);

    impl NodeKey {
        fn as_usize(self) -> usize {
            self.0.get() - 1
        }

        pub(super) fn insert(slab: &mut Slab<Node>, val: Node) -> Self {
            let raw = slab.insert(val);
            NodeKey(unsafe { NonZeroUsize::new_unchecked(raw + 1) })
        }

        pub(super) fn get(self, slab: &Slab<Node>) -> &Node {
            slab.get(self.as_usize()).unwrap()
        }

        pub(super) fn get_mut(self, slab: &mut Slab<Node>) -> &mut Node {
            slab.get_mut(self.as_usize()).unwrap()
        }

        pub(super) fn remove(self, slab: &mut Slab<Node>) -> Node {
            slab.remove(self.as_usize())
        }
    }
}

use keys::*;

#[derive(Clone)]
struct Node {
    prefix: SmallVec<[u8; 8]>,
    val: Option<ItemKey>,
    slots: [Option<NodeKey>; 256],
}

impl Node {
    fn new() -> Self {
        Node {
            prefix: SmallVec::new(),
            val: None,
            slots: [None; 256],
        }
    }

    fn is_leaf(&self) -> bool {
        self.slots.iter().all(|o| o.is_none())
    }
}

struct WalkTo<'a, 'b> {
    curr: &'a Node,
    // keys[i].slots[branches[i+1]] = keys[i+1]
    // root.slots[branches[0]] = keys[0]
    trail_keys: Vec<NodeKey>,
    trail_branches: Vec<u8>,
    shared: &'b [u8],
    rest: &'b [u8],
}

impl<'a, 'b> WalkTo<'a, 'b> {
    fn last_key(&self) -> Option<NodeKey> {
        Some(*self.trail_keys.last()?)
    }
}

impl<T> Trie<T> {
    #[inline]
    fn walk_to<'a, 'b>(&'a self, key: &'b [u8]) -> WalkTo<'a, 'b> {
        let mut trail_keys = Vec::with_capacity(key.len());
        let mut trail_branches = Vec::with_capacity(key.len());
        let mut curr = &self.root;
        let mut rest = key;

        loop {
            let pref = curr.prefix.as_slice();
            let shared_len = shared_len(pref, rest);
            let (shared, suff) = rest.split_at(shared_len);

            if !suff.is_empty() && pref.len() == shared.len() {
                debug_assert!(pref == shared);

                trail_branches.push(suff[0]);
                if let Some(ix) = curr.slots[suff[0] as usize] {
                    // recurse
                    trail_keys.push(ix);
                    curr = ix.get(&self.node_slab);
                    rest = &suff[1..];
                    continue;
                }
            }

            // dead end
            return WalkTo {
                curr,
                trail_keys,
                trail_branches,
                shared,
                rest,
            };
        }
    }

    pub fn get<'a>(&'a self, key: &[u8]) -> Option<&'a T> {
        let WalkTo {
            shared, rest, curr, ..
        } = self.walk_to(key);

        if rest.is_empty() && shared.len() == curr.prefix.len() {
            let ix = curr.val?;
            Some(ix.get(&self.item_slab))
        } else {
            None
        }
    }

    pub fn get_mut<'a>(&'a mut self, key: &[u8]) -> Option<&'a mut T> {
        let WalkTo {
            shared, rest, curr, ..
        } = self.walk_to(key);

        if rest.is_empty() && shared.len() == curr.prefix.len() {
            let ix = curr.val?;
            Some(ix.get_mut(&mut self.item_slab))
        } else {
            None
        }
    }

    pub fn insert(&mut self, key: &[u8], val: T) -> Option<T> {
        let walked = self.walk_to(key);
        let last_ix = walked.last_key();
        let WalkTo {
            shared, rest, curr, ..
        } = walked;

        if shared.len() == curr.prefix.len() {
            // no need to split curr
            if let Some((d, suff)) = rest.split_first() {
                debug_assert!(curr.slots[*d as usize].is_none());

                // first we'll allocate the new item
                let vix = ItemKey::insert(&mut self.item_slab, val);

                // next we'll make a new node
                let mut new_node = Node::new();
                new_node.val = Some(vix);
                new_node.prefix = suff.into();

                // next we allocate it
                let nix = NodeKey::insert(&mut self.node_slab, new_node);

                // get a mutable ref to current node
                let node_ref = if let Some(ix) = last_ix {
                    ix.get_mut(&mut self.node_slab)
                } else {
                    &mut self.root
                };

                // and place the ix
                let old_nix = node_ref.slots[*d as usize].replace(nix);

                // if somehow old_nix is not none, we should at least delete the item associated with it
                debug_assert!(old_nix.is_none());

                None
            } else if let Some(ix) = curr.val {
                // if we already have a slot we can reuse it
                Some(std::mem::replace(ix.get_mut(&mut self.item_slab), val))
            } else {
                // get a mutable ref to current node
                let node_ref = if let Some(ix) = last_ix {
                    ix.get_mut(&mut self.node_slab)
                } else {
                    &mut self.root
                };

                // then allocate the new item
                let vix = ItemKey::insert(&mut self.item_slab, val);

                // and place it
                let old_vix = node_ref.val.replace(vix);

                // if somehow old_vix is not none, we should at least delete the item associated with it
                debug_assert!(old_vix.is_none());

                None
            }
        } else {
            // we need to split curr
            assert!(shared.len() < curr.prefix.len());
            let (old_dig, old_suff) = curr.prefix[shared.len()..].split_first().unwrap();
            let old_dig = *old_dig;

            let mut new_old = curr.clone();
            new_old.prefix = old_suff.into();
            let old_ix = NodeKey::insert(&mut self.node_slab, new_old);

            // allocate the new item
            let vix = ItemKey::insert(&mut self.item_slab, val);

            let mut new_curr = Node::new();
            new_curr.prefix = shared.into();
            new_curr.slots[old_dig as usize].replace(old_ix);

            if let Some((new_dig, new_suff)) = rest.split_first() {
                debug_assert!(*new_dig != old_dig);

                let mut new_node = Node::new();
                new_node.prefix = new_suff.into();
                new_node.val = Some(vix);

                // place the new node
                let new_ix = NodeKey::insert(&mut self.node_slab, new_node);

                new_curr.slots[*new_dig as usize].replace(new_ix);
            } else {
                new_curr.val = Some(vix);
            }

            // get a mutable ref to current node
            let node_ref = if let Some(ix) = last_ix {
                ix.get_mut(&mut self.node_slab)
            } else {
                &mut self.root
            };

            *node_ref = new_curr;

            None
        }
    }

    pub fn remove(&mut self, key: &[u8]) -> Option<T> {
        let WalkTo {
            shared,
            rest,
            curr,
            mut trail_keys,
            mut trail_branches,
        } = self.walk_to(key);

        if !rest.is_empty() || shared.len() != curr.prefix.len() || curr.val.is_none() {
            return None;
        }

        let vix = curr.val.unwrap();

        // we have to propagate the deletion
        // but only if there was a node to delete
        if let Some(last_key) = trail_keys.pop() {
            // if it's not a leaf there's nothing to delete
            if curr.is_leaf() {
                last_key.remove(&mut self.node_slab);

                while let Some(branch) = trail_branches.pop() {
                    if let Some(nkey) = trail_keys.pop() {
                        let node = nkey.get_mut(&mut self.node_slab);
                        let old = node.slots[branch as usize].take();
                        debug_assert!(old.is_some());
                        if node.val.is_some() {
                            // there's a value here, nothing left to delete
                            break;
                        } else {
                            let mut full_slots = node
                                .slots
                                .iter()
                                .enumerate()
                                .filter_map(|(d, o)| o.map(|b| (d, b)))
                                .take(2);
                            if let Some((branch, first_child)) = full_slots.next() {
                                if full_slots.next().is_none() {
                                    // it has only 1 child, compress the trie
                                    // swap the prefix out - we'll be extending it and putting it back in place
                                    let mut prefix = std::mem::take(&mut node.prefix);
                                    prefix.push(branch as u8);

                                    let Node {
                                        prefix: child_prefix,
                                        val: child_val,
                                        slots: child_slots,
                                    } = first_child.remove(&mut self.node_slab);

                                    prefix.extend_from_slice(&child_prefix);

                                    let node = nkey.get_mut(&mut self.node_slab);
                                    node.prefix = prefix;
                                    node.val = child_val;
                                    node.slots = child_slots;
                                }
                                break;
                            } else {
                                // this node is now an empty leaf, delete it
                                nkey.remove(&mut self.node_slab);
                            }
                        }
                    } else {
                        let old = self.root.slots[branch as usize].take();
                        debug_assert!(old.is_some());
                        assert!(trail_branches.pop().is_none());
                    }
                }
            }
        }

        Some(vix.remove(&mut self.item_slab))
    }
}

fn shared_len<T: Eq>(a: &[T], b: &[T]) -> usize {
    let mut i = 0;

    loop {
        match (a.get(i), b.get(i)) {
            (Some(l), Some(r)) if l == r => {
                i += 1;
            }
            _ => return i,
        }
    }
}
