/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.ngqlbuilder.entity;

import java.util.ArrayList;
import java.util.Map;
import java.util.Objects;

/**
 * {@link #Relationship} can be used to create an edge between two points,
 * and the edge has a direction.
 *
 * <p>you can create an edge by passing in {@link #startVid} and {@link #endVid},
 * the name of the edge type ({@link #edgeName}), rank ({@link #rank}) (the default value is 0,
 * rank value can be used to distinguish edges with the same edge type,
 * starting point and destination point)
 * and the attributes of the edge ({@link #propMap}).</p>
 *
 * @author Qi Kai Meng
 */
public class Relationship extends Entity {
    private Object startVid;
    private Object endVid;
    private String edgeName;
    private int rank = 0;
    private Map<String, Object> propMap = null;

    /**
     * user pass in parameter creates a directional edge.
     *
     * @param startVid startVid
     * @param endVid   endVid
     * @param edgeName edgeName
     * @param propMap  attribute Map
     * @param rank     distinguish edges with the same edge type, starting point and destination point.
     */
    public Relationship(Object startVid, Object endVid, String edgeName,
                        Map<String, Object> propMap, int rank) {
        this.startVid = startVid;
        this.endVid = endVid;
        this.edgeName = edgeName;
        this.propMap = propMap;
        this.rank = rank;
    }

    /**
     * user pass in parameter creates a directional edge,rank use default value.
     *
     * @param startVid startVid
     * @param endVid   endVid
     * @param edgeName edgeName
     * @param propMap  attribute Map
     */
    public Relationship(Object startVid, Object endVid, String edgeName,
                        Map<String, Object> propMap) {
        this.startVid = startVid;
        this.endVid = endVid;
        this.edgeName = edgeName;
        this.propMap = propMap;
    }

    /**
     * create relationship and edgeType propertyList is null.
     */
    public Relationship(Object startVid, Object endVid, String edgeName, int rank) {
        this.startVid = startVid;
        this.endVid = endVid;
        this.edgeName = edgeName;
        this.rank = rank;
    }

    /**
     * create relationship and edgeType propertyList is null,rank use default value.
     */
    public Relationship(Object startVid, Object endVid, String edgeName) {
        this.startVid = startVid;
        this.endVid = endVid;
        this.edgeName = edgeName;
    }

    public String getEdgeName() {
        return edgeName;
    }

    public void setRank(int rank) {
        this.rank = rank;
    }

    public void setStartVid(Object startVid) {
        this.startVid = startVid;
    }

    public void setEndVid(Object endVid) {
        this.endVid = endVid;
    }

    public void setEdgeName(String edgeName) {
        this.edgeName = edgeName;
    }

    public void setPropMap(Map<String, Object> propMap) {
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
            && Objects.equals(startVid, that.startVid)
            && Objects.equals(endVid, that.endVid)
            && Objects.equals(edgeName, that.edgeName);
    }

    @Override
    public int hashCode() {
        return Objects.hash(startVid, endVid, edgeName, rank, propMap);
    }

    public Map<String, Object> properties() {
        return propMap;
    }

    public Map<String, Object> getPropMap() {
        return propMap;
    }

    public Object getStartVid() {
        return startVid;
    }

    public Object getEndVid() {
        return endVid;
    }

    public int getRank() {
        return rank;
    }

    @Override
    public String toString() {
        //("1")-[:p_t_r@0{startTime: 2021-03-05, salve: 34}]->("4")
        //(1)-[:friend@1{name: "wer"}]->(2)
        String result = "(%s)-[:%s@%s{%s}]->(%s)";
        ArrayList<String> prop = new ArrayList<>();
        StringBuilder propValue = new StringBuilder();
        if (propMap != null && !propMap.isEmpty()) {
            for (String propName : propMap.keySet()) {
                if (propMap.get(propName) instanceof String) {
                    prop.add(String.format("%s: \"%s\"", propName, propMap.get(propName)));
                } else {
                    prop.add(String.format("%s: %s", propName, propMap.get(propName)));
                }
            }
            propValue.append(String.join(", ", prop));
        }
        return String.format(result,
            (startVid instanceof String) ? "\"" + startVid + "\"" : startVid,
            edgeName, rank, propValue,
            (endVid instanceof String) ? "\"" + endVid + "\"" : endVid);
    }
}