/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */
package entity;

import java.util.ArrayList;
import java.util.List;

public class Walkable extends Subgraph{

    //vertex and edges are stored alternately
    private List<Object> sequence;

    protected Walkable(){

    }

    public Walkable(List<Object> sequence) {
          init(sequence);
    }

    protected  void init(List<Object> sequence){
        this.sequence = sequence;
        List<Vertex> vertexList = new ArrayList<>();
        List<Relationship> relationshipList = new ArrayList<>();

        //store points and edges separately in vertexList and relationshipList
        init(vertexList,relationshipList);
    }

    public Vertex getStartNode(){
        return (Vertex) sequence.get(0);
    }

    public Vertex getEndNode(){

        return (Vertex) sequence.get(sequence.size()-1);
    }

    //get all nodes of subgraph
    public List<Vertex> getNodes(){
        return vertexes();
    }

    //get all relationships of subgraph
    public List<Relationship> getRelationships(){
        return relationships();
    }

    public int steps(){
        return sequence.size()-1;
    }



}
