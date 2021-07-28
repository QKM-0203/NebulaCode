/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */
package entity;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Objects;

public class Relationship extends Entity{

    private Object startVertex;

    private Object endVertex;

    private String edgeName;

    private int rank = 0;

    private HashMap<String,Object>  propMap;



    public Relationship(Object startVertex, Object endVertex, String edgeName, HashMap<String, Object> propMap, int rank) {
        this.startVertex = startVertex;
        this.endVertex = endVertex;
        this.edgeName = edgeName;
        this.propMap = propMap;
        this.rank = rank;
    }

    /**
     * edgeType propertyList is null
     */
    public Relationship(Object startVertex, Object endVertex, String edgeName, int rank) {
        this.startVertex = startVertex;
        this.endVertex = endVertex;
        this.edgeName = edgeName;
        this.rank = rank;
    }

    public String getEdgeName() {
        return edgeName;
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

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        Relationship that = (Relationship) o;
        return rank == that.rank
                && Objects.equals(startVertex, that.startVertex)
                && Objects.equals(endVertex, that.endVertex)
                && Objects.equals(edgeName, that.edgeName);
    }

    @Override
    public int hashCode() {
        return Objects.hash(startVertex, endVertex, edgeName, rank, propMap);
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

    public HashMap<String, Object> getPropMap() {
        return propMap;
    }

    public Object getStartVertex() {
        return startVertex;
    }

    public Object getEndVertex() {
        return endVertex;
    }

    public int getRank() {
        return rank;
    }

   //("1")-[:p_t_r@0{startTime: 2021-03-05, salve: 34}]->("4")
    //(1)-[:friend@1{name: "wer"}]->(2)
    @Override
    public String toString() {
        String result = "(%s)"+ "-" + "[:" +edgeName+"@"+ rank + "%s" + "]" + "->" +"(%s)";
        ArrayList<String> prop = new ArrayList<>();
        StringBuilder part = new StringBuilder("{");
        for (String propName : propMap.keySet()) {
             if(propMap.get(propName) instanceof  String){
                 prop.add(String.format("%s: "+"\""+"%s"+"\"",propName,propMap.get(propName)));
             }else{
                 prop.add(String.format("%s: "+"%s",propName,propMap.get(propName)));
             }
        }

        return String.format(result,
                (startVertex instanceof String)? "\""+startVertex+"\"":startVertex,
                part.append(String.join(", ",prop)).append("}"),
                (endVertex instanceof String)? "\""+endVertex+"\"":endVertex);
    }
}
