/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */
package entity;

import java.util.HashMap;

public class Relationship extends Entity{

    private Vertex startVertex;

    private Vertex endVertex;

    private String edgeName;

    private int rank = 0;

    private HashMap<String,Object>  propMap;

    //only one vertex and null edgeType propertyList
    public Relationship(Vertex startVertex, String edgeName, int rank) {
        this.startVertex = startVertex;
        this.edgeName = edgeName;
        this.rank = rank;
    }

    //only one vertex and edgeType propertyList
    public Relationship(Vertex startVertex, String edgeName, HashMap<String, Object> propMap, int rank) {
        this.startVertex = startVertex;
        this.edgeName = edgeName;
        this.propMap = propMap;
        this.rank = rank;
    }


    public Relationship(Vertex startVertex, Vertex endVertex, String edgeName, HashMap<String, Object> propMap, int rank) {
        this.startVertex = startVertex;
        this.endVertex = endVertex;
        this.edgeName = edgeName;
        this.propMap = propMap;
        this.rank = rank;
    }

    //edgeType propertyList is null
    public Relationship(Vertex startVertex, Vertex endVertex, String edgeName, int rank) {
        this.startVertex = startVertex;
        this.endVertex = endVertex;
        this.edgeName = edgeName;
        this.rank = rank;
    }


    public void setRank(int rank) {
        this.rank = rank;
    }

    public void setStartVertex(Vertex startVertex) {
        this.startVertex = startVertex;
    }

    public void setEndVertex(Vertex endVertex) {
        this.endVertex = endVertex;
    }

    public void setEdgeName(String edgeName) {
        this.edgeName = edgeName;
    }

    public void setPropMap(HashMap<String, Object> propMap) {
        this.propMap = propMap;
    }

    public Graph graph(){
        return graph;
    }

    public HashMap<String,Object> properties(){
        return propMap;
    }

    public boolean updateProp(HashMap<String,Object> propMap){
        return true;
    }


}
