/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */
package entity;

import java.util.HashMap;

/**
 * this class {@code Relationship} is used to create Relationship objects.
 *
 * <p>the user can pass in parameters to the {@link #Relationship} constructor to create Relationship Object
 * or create an empty constructor and pass the value through the set series of method.
 *
 * @author Qi Kai Meng
 * @see HashMap
 * @since 21 July 2021
 *
 *
 *
 */
public class Relationship {
    private Object srcVid;

    private Object dstVid;

    private String edgeName;

    private int rank = 0;

    //propMap
    private HashMap<String, Object> propMap;

    public Relationship() {
    }

    public Relationship(Object srcVid, Object dstVid, String edgeName, HashMap<String, Object> propMap) {
        this.srcVid = srcVid;
        this.dstVid = dstVid;
        this.edgeName = edgeName;
        this.propMap = propMap;
    }

    public Relationship(Object srcVid, Object dstVid, String edgeName, int rank, HashMap<String, Object> propMap) {
        this.srcVid = srcVid;
        this.dstVid = dstVid;
        this.edgeName = edgeName;
        this.rank = rank;
        this.propMap = propMap;
    }

    public void setSrcVid(Object srcVid) {
        this.srcVid = srcVid;
    }

    public void setDstVid(Object dstVid) {
        this.dstVid = dstVid;
    }

    public void setRank(int rank) {
        this.rank = rank;
    }

    public void setPropMap(HashMap<String, Object> propMap) {
        this.propMap = propMap;
    }

    public void setEdgeName(String edgeName) {
        this.edgeName = edgeName;
    }


}
