// Pull in desired css/stylus files
require( '../styles/app.styl' );

// Call Entry point
var Entry = require( '../../out/Program' );

 function tree_add_leaf_example_click(leaf, node, pnode, tree){
        tree.addLeaf(node, "Leaf item");
    }