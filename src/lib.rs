type Slot<T> = Option<Box<Node<T>>>;

pub struct Node<T> {
    prefix: Vec<u8>,
    val: Option<T>,
    items: [Slot<T>; 256],
}

impl<T> Node<T> {
    fn new() -> Self {
        // we have to use uninit here because no const generics :(
        use std::mem::MaybeUninit;

        let mut items: [MaybeUninit<Slot<T>>; 256] = unsafe { MaybeUninit::uninit().assume_init() };
        for item in items.iter_mut() {
            *item = MaybeUninit::new(None)
        }

        let items = unsafe { std::mem::transmute(items) };
        // let mut uninit: MaybeUninit<[Option<Box<Node<T>>>; 256]> = MaybeUninit::uninit();

        Node {
            prefix: Vec::new(),
            val: None,
            items,
        }
    }

    pub fn get<'a>(&'a self, key: &[u8]) -> Option<&'a T> {
        let WalkTo { curr, shared, rest } = self.walk_to(key);

        if rest == b"" && shared.len() == curr.prefix.len() {
            curr.val.as_ref()
        } else {
            None
        }
    }

    pub fn get_mut<'a>(&'a mut self, key: &[u8]) -> Option<&'a mut T> {
        let WalkToMut { curr, shared, rest } = self.walk_to_mut(key);

        if rest == b"" && shared.len() == curr.prefix.len() {
            curr.val.as_mut()
        } else {
            None
        }
    }

    pub fn insert(&mut self, key: &[u8], val: T) -> Option<T> {
        let WalkToMut { curr, shared, rest } = self.walk_to_mut(key);

        if shared.len() == curr.prefix.len() {
            // no need to split
            if rest.is_empty() {
                // insert here
                curr.val.replace(val)
            } else {
                // insert 1 down
                let (d, suff) = rest.split_first().unwrap();
                let mut new = Self::new();

                new.prefix = suff.to_vec();
                new.val.replace(val);

                curr.items[*d as usize].replace(Box::new(new));

                None
            }
        } else {
            debug_assert!(shared.len() < curr.prefix.len());

            let mut new_curr = Self::new();
            new_curr.prefix = shared.to_vec();

            if rest.is_empty() {
                new_curr.val.replace(val);
            } else {
                let mut new_loc = Self::new();
                new_loc.prefix = rest[1..].to_vec();
                new_loc.val.replace(val);

                new_curr.items[rest[0] as usize].replace(Box::new(new_loc));
            }

            // next we swap new_curr into place
            std::mem::swap(curr, &mut new_curr);
            let mut old_curr = new_curr;

            // and finally we put old_curr in its rightful location under new_curr
            let d = old_curr.prefix[shared.len()];
            let suff = old_curr.prefix[shared.len() + 1..].to_vec();
            old_curr.prefix = suff;
            curr.items[d as usize].replace(Box::new(old_curr));

            None
        }
    }
}

macro_rules! walk_to_body {
    ($this: expr, $key: expr, $ctor: tt,  $as_deref: tt) => {
        let mut curr = $this;
        let mut rest = $key;

        loop {
            let pref = curr.prefix.as_slice();
            let shared_len = shared_len(pref, rest);
            let (shared, suff) = rest.split_at(shared_len);

            if !suff.is_empty() && pref == shared && curr.items[suff[0] as usize].is_some() {
                // traverse
                let next = curr.items[suff[0] as usize].$as_deref().unwrap();
                curr = next;
                rest = &suff[1..];
            } else {
                // dead end
                return $ctor {
                    curr,
                    shared,
                    rest: suff,
                };
            }
        }
    };
}

impl<T> Node<T> {
    #[inline]
    fn walk_to<'a, 'b>(&'a self, key: &'b [u8]) -> WalkTo<'a, 'b, T> {
        walk_to_body!(self, key, WalkTo, as_deref);
    }

    #[inline]
    fn walk_to_mut<'a, 'b>(&'a mut self, key: &'b [u8]) -> WalkToMut<'a, 'b, T> {
        walk_to_body!(self, key, WalkToMut, as_deref_mut);
    }
}

struct WalkTo<'a, 'b, T> {
    curr: &'a Node<T>,
    shared: &'b [u8],
    rest: &'b [u8],
}

struct WalkToMut<'a, 'b, T> {
    curr: &'a mut Node<T>,
    shared: &'b [u8],
    rest: &'b [u8],
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
