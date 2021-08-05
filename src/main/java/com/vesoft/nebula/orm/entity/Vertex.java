/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.entity;

import com.vesoft.nebula.orm.exception.InitException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

/**
 * vertex object with ID and attribute.
 *
 * <p>you can create a point by passing in the ID of the point and
 * the tag attribute list of the point,and then create the point
 * according to the graph object.</p>
 *
 * <p>judge whether the tag exists({@link #hasTag(String)}),
 * add a new tag({@link #addTag(String, HashMap)}), etc</p>
 *
 * @author Qi Kai Meng
 */
public class Vertex extends Entity {
    private Object vid;
    private HashMap<String, HashMap<String, Object>> propMap;

    public Vertex(Object vid, HashMap<String, HashMap<String, Object>> propMap) {
        this.vid = vid;
        if (propMap == null || propMap.isEmpty()) {
            throw new InitException("the vertex contains at least one tag");
        }
        this.propMap = propMap;
    }

    public HashMap<String, HashMap<String, Object>> getPropMap() {
        return propMap;
    }

    public void setPropMap(HashMap<String, HashMap<String, Object>> propMap) {
        this.propMap = propMap;
    }

    public Object getVid() {
        return vid;
    }

    public void setVid(Object vid) {
        this.vid = vid;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        Vertex vertex = (Vertex) o;
        return Objects.equals(vid, vertex.vid);
    }

    @Override
    public int hashCode() {
        return Objects.hash(vid, propMap);
    }

    /**
     * <p>if users want to get the latest remote data,  can use {@link Graph#pull}</p>
     *
     * @param tagName tagName
     * @return specify the properties of the tag
     */
    public HashMap<String, Object> getTag(String tagName) {
        return this.getPropMap().get(tagName);
    }


    /**
     * <p>if users want to get the latest remote data,  can use {@link Graph#pull}</p>
     *
     * @return all tagNames
     */
    public List<String> getTagNames() {
        return new ArrayList<>(this.getPropMap().keySet());
    }

    /**
     * <p>if users want to get the latest remote data,  can use {@link Graph#pull}</p>
     *
     * @return true or false
     */
    public boolean hasTag(String tagName) {
        return this.getPropMap().containsKey(tagName);
    }

    /**
     * pass in tagName and attribute Map to add tag for vertex,
     * if want to update to remote database use {@link Graph#push}
     *
     * @param name    tagName
     * @param propMap attribute Map
     */
    public void addTag(String name, HashMap<String, Object> propMap) {
        this.getPropMap().put(name, propMap);
    }

    @Override
    public Graph getGraph() {
        return super.getGraph();
    }

    /**
     * this operation means deleting the vertex,
     * If you want to update to the remote, use {@link Graph#push}
     */
    public void clearAllTags() {
        this.getPropMap().clear();
    }

    @Override
    public String toString() {
        //(1 :QKM3{name: "asd", age: 19} :QKM2{name: "asd", age: 19})
        String result = (vid instanceof String) ? "(\"%s\"%s)" : "(%s%s)";
        StringBuilder part = new StringBuilder();
        for (String tagName : propMap.keySet()) {
            HashMap<String, Object> propValueMap = propMap.get(tagName);
            part.append(String.format(" :%s{", tagName));
            if (propValueMap == null || propValueMap.isEmpty()) {
                part.append("}");
            } else {
                ArrayList<String> prop = new ArrayList<>();
                for (String propName : propValueMap.keySet()) {
                    if (propValueMap.get(propName) instanceof String) {
                        prop.add(String.format("%s: \"%s\"", propName, propValueMap.get(propName)));
                    } else {
                        prop.add(String.format("%s: %s", propName, propValueMap.get(propName)));
                    }
                }
                part.append(String.join(", ", prop)).append("}");
            }
        }
        return String.format(result, vid, part);
    }
}