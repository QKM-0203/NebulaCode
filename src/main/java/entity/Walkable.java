/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */
package entity;
import java.util.List;

public class Walkable {

    private List<Relationship> relationships;
    private List<Vertex> vertices;


    protected void init(List<Relationship> relationships,List<Vertex> vertices){
        this.relationships = relationships;
        this.vertices = vertices;
    }

    public Vertex getStartNode(){
        return vertices.get(0);
    }

    public Vertex getEndNode(){
        return vertices.get(vertices.size()-1);
    }

    /**
     * get all vertices
     */
    public List<Vertex> getVertices(){
        return vertices;
    }

    /**
     * get all relationships
     */
    public List<Relationship> getRelationships(){
        return relationships;
    }

    public int steps(){
        return relationships.size();
    }



}
