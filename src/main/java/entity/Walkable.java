/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */
package entity;

import java.util.List;

public class Walkable {

   private Subgraph subgraph;

    public Walkable(Subgraph subgraph) {
        this.subgraph = subgraph;
    }

    public void setSubgraph(Subgraph subgraph) {
        this.subgraph = subgraph;
    }

    //get id or all
    public Vertex getStartNode(){
        return null;
    }

    public Vertex getEndNode(){
        return null;
    }

    //get all nodes of subgraph
    public List<Vertex> getNodes(){
        return null;
    }

    //get all relationships of subgraph
    public List<Relationship> getRelationships(){
        return null;
    }



}
