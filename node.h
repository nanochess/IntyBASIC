//
//  node.h
//  intybasic
//
//  Created by Oscar Toledo on 07/01/16.
//  Copyright (c) 2016 Oscar Toledo. All rights reserved.
//

#ifndef _intybasic_node_
#define _intybasic_node_

//
// Expression tree builder
//
class node {
    enum lexical_component type;
    int value;
    int regs;
    class node *left;
    class node *right;
    
public:
    node(enum lexical_component type, int value, class node *left, class node *right);
    ~node();
    enum lexical_component node_type(void);
    int node_value(void);
    class node *node_left(void);
    class node *node_right(void);
    void set_right(class node *right);
    bool valid_array(void);
    void annotate_index_for_subexpression(void);
    void label(void);
    void generate(int reg, int decision);
};
    
#endif /* defined(_intybasic_node_) */
