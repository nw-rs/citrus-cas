use crate::token::Token;
use raw_pointer::Pointer;

/// a node in the expression tree
#[derive(Clone, Copy)]
pub struct ExpressionNode<const CHILDREN: usize> {
    pub value: Token,
    children: [Option<Pointer<ExpressionNode<CHILDREN>>>; CHILDREN],
    parent: Option<Pointer<ExpressionNode<CHILDREN>>>,
    len: usize,
}

impl<const C: usize> PartialEq for ExpressionNode<C> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value && self.len == other.len
    }
}

impl<const C: usize>  Eq for ExpressionNode<C> {}

impl<const C: usize> ExpressionNode<C> {
    pub fn default() -> Self {
        ExpressionNode {
            value: Token::Terminator,
            children: [None; C],
            parent: None,
            len: 0,
        }
    }
    pub fn add_child(&mut self, child: Pointer<ExpressionNode<C>>) {
        self.children[self.len] = Some(child);
        self.len += 1;
    }
    pub fn remove_child(&mut self, child: Pointer<ExpressionNode<C>>) -> Pointer<ExpressionNode<C>> {
        let id = self.children.iter().position(|x| x.unwrap().unwrap() == child.unwrap()).unwrap();
        self.len -= 1;
        self.children[id] = self.children[self.len];
        child
    }
    pub fn get_child(&self, id: usize) -> Pointer<ExpressionNode<C>> {
        self.children[id].unwrap()
    }
    //awful
    pub fn get_children(&self) -> [Option<Pointer<ExpressionNode<C>>>; C] {
        self.children
    }
    pub fn get_parent(&self) -> Option<Pointer<ExpressionNode<C>>> {
        self.parent
    }
    pub fn reassign_parent(&mut self, parent: Pointer<ExpressionNode<C>>) {
        self.parent = Some(parent);
    }
    pub fn capacity(&self) -> usize {
        C
    }
    pub fn len(&self) -> usize {
        self.len
    }
}

/// an abstract syntax tree that represents a mathematical expression
/// structured for being heapless and safe
pub struct ExpressionTree<const LENGTH: usize, const CHILDREN: usize> {
    pub nodes: [ExpressionNode<CHILDREN>; LENGTH],
    pub root: Pointer<ExpressionNode<CHILDREN>>,
    len: usize,
}

impl<const L: usize, const C: usize> ExpressionTree<L, C> {
    pub fn new() -> Self {
        Self {
            nodes: [ExpressionNode::<C>::default(); L],
            root: Pointer::new(&mut ExpressionNode::<C>::default()),
            len: 0,
        }
    }
    pub fn add_node(&mut self, mut parent: Pointer<ExpressionNode<C>>, child: ExpressionNode<C>) -> Result<(), ExpressionNode<C>> {
        if self.len() == 0 {
            self.len += 1;

            self.nodes[0] = child.clone();
            self.root = Pointer::new(&mut self.nodes[0]);

            Ok(())
        }
        else if self.len() < self.capacity() {
            self.len += 1;

            self.nodes[self.len()] = child.clone();
            parent.add_child(Pointer::<ExpressionNode<C>>::new(&mut self.nodes[self.len]));

            Ok(())
        } else {
            Err(child)
        }
    }
    pub fn remove_branch(&mut self, node: Pointer<ExpressionNode<C>>) -> Result<(), Pointer<ExpressionNode<C>>> {
        if !self.contains(node) {
            return Err(node);
        }

        //because this recursively removes children from this node, we need to clone its children
        let children = node.unwrap().get_children().clone();
        children.iter().for_each(|child| {
            if let Some(c) = child {
                self.remove_branch(*c);
            }
        });

        //lazy deletion (send node to a place where it will be overwritten)
        node.unwrap().get_parent().unwrap().remove_child(node);
        self.len -= 1;
        self.swap_branch(node, Pointer::new(&mut self.nodes[self.len()+1])); 

        Ok(())
    }
    pub fn swap_branch(&mut self, node1: Pointer<ExpressionNode<C>>, node2: Pointer<ExpressionNode<C>>) -> Result<(), Pointer<ExpressionNode<C>>> {
        if !(self.contains(node1) && self.contains(node2)) {
            return Err(node1);
        }
        
        let children2 = node2.unwrap().get_children().clone();
        
        node1.unwrap().get_children().iter().for_each(|child| {
            if let Some(c) = child {
                c.reassign_parent(node2);
            }
        });

        for child in children2 {
            if let Some(c) = child {
                c.reassign_parent(node1);
            }
        }

        Ok(())
    }
    pub fn give_children(&mut self, node: Pointer<ExpressionNode<C>>, new_node: Pointer<ExpressionNode<C>>) -> Result<(), Pointer<ExpressionNode<C>>> {
        if !(self.contains(node) && self.contains(new_node)) {
            return Err(node);
        }

        node.unwrap().get_children().iter().for_each(|child| {
            if let Some(c) = child {
                c.reassign_parent(new_node);
            }
        });

        Ok(())
    }
    pub fn reassign_branch(&mut self, node: Pointer<ExpressionNode<C>>, new_node: Pointer<ExpressionNode<C>>) -> Result<(), Pointer<ExpressionNode<C>>> {
        if !(self.contains(node) && self.contains(new_node)) {
            return Err(node);
        }

        self.give_children(node, new_node)?;
        self.remove_branch(node);

        Ok(())
    }
    //there must be a more efficient way of doing this, bc we should know if Pointer directs towards the memory of nodes[]
    pub fn contains(&self, node: Pointer<ExpressionNode<C>>) -> bool {
        //we assume node is aligned, but it may not be
        if node.as_ptr().is_null() { 
            return false;
        }

        self.nodes.iter().any(|x| x == node.unwrap())
    }
    pub fn capacity(&self) -> usize {
        L
    }
    pub fn len(&self) -> usize {
        self.len
    }
}