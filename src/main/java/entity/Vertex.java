/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */
package entity;


import java.util.*;


public class Vertex {

    //vertexVid
    private Object vid;


    private HashMap<String, HashMap<String,Object>> propMap;


    public Vertex(Object vid, HashMap<String, HashMap<String, Object>> propMap) {
        this.vid = vid;
        this.propMap = propMap;
    }

    public void setVid(Object vid) {
        this.vid = vid;
    }

    public void setPropMap(HashMap<String, HashMap<String, Object>> propMap) {
        this.propMap = propMap;
    }
}
