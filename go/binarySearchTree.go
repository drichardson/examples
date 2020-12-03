package main

import (
	"fmt"
)

type TreeNode struct {
	left *TreeNode
	right *TreeNode
	key string
}

func AddKey(tree *TreeNode, key string) *TreeNode {
	if (tree == nil) {
		var result = new(TreeNode)
		result.key = key
		return result
	}

	if (tree.key > key) {
		tree.right = AddKey(tree.right, key)
		return tree
	} else if (tree.key < key) {
		tree.left = AddKey(tree.left, key)
		return tree
	} else {
		// do nothing, key already there.
		return tree
	}
}

func Visit(tree *TreeNode, visitor func(key string)) {
	if (tree == nil) {
		return
	}

	Visit(tree.left, visitor)
	visitor(tree.key)
	Visit(tree.right, visitor)
}

func main() {
	t := AddKey(nil, "doug")
	t = AddKey(t, "rebecca")
	t = AddKey(t, "jack")
	t = AddKey(t, "jack")
	t = AddKey(t, "jack")
	t = AddKey(t, "zack")
	t = AddKey(t, "ack")

	f := func(key string) {
		fmt.Println("Visiting ", key)
	}

	Visit(t, f)
}
