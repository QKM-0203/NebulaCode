/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */
package entity;

import java.util.HashMap;

public class Relationship {

    private Vertex  leftVertex;

    private Vertex  rightVertex;

    private String edgeName;

    private int rank = 0;

    private HashMap<String,Object>  propMap;

    //only one vertex and null edgeType propertyList
    public Relationship(Vertex leftVertex,String edgeName,int rank) {
        this.leftVertex = leftVertex;
        this.edgeName = edgeName;
        this.rank = rank;
    }

    //only one vertex and edgeType propertyList
    public Relationship(Vertex leftVertex, String edgeName, HashMap<String, Object> propMap,int rank) {
        this.leftVertex = leftVertex;
        this.edgeName = edgeName;
        this.propMap = propMap;
        this.rank = rank;
    }


    public Relationship(Vertex leftVertex, Vertex rightVertex, String edgeName, HashMap<String, Object> propMap,int rank) {
        this.leftVertex = leftVertex;
        this.rightVertex = rightVertex;
        this.edgeName = edgeName;
        this.propMap = propMap;
        this.rank = rank;
    }

    //edgeType propertyList is null
    public Relationship(Vertex leftVertex, Vertex rightVertex, String edgeName,int rank) {
        this.leftVertex = leftVertex;
        this.rightVertex = rightVertex;
        this.edgeName = edgeName;
        this.rank = rank;
    }




    public void setRank(int rank) {
        this.rank = rank;
    }

    public void setLeftVertex(Vertex leftVertex) {
        this.leftVertex = leftVertex;
    }

    public void setRightVertex(Vertex rightVertex) {
        this.rightVertex = rightVertex;
    }

    public void setEdgeName(String edgeName) {
        this.edgeName = edgeName;
    }

    public void setPropMap(HashMap<String, Object> propMap) {
        this.propMap = propMap;
    }
}
