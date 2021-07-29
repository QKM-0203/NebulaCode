/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package entity;

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
 * <p>you can update the tag attribute of the vertex({@link #updateTag(String, HashMap)}),
 * judge whether the tag exists({@link #hasTag(String)}),
 * add a new tag({@link #addTag(String, HashMap)}), etc</p>
 *
 */
public class Vertex extends Entity {
    private Object vid;
    private HashMap<String, HashMap<String, Object>> propMap;

    public Vertex(Object vid, HashMap<String, HashMap<String, Object>> propMap) {
        this.vid = vid;
        this.propMap = propMap;
    }

    public HashMap<String, HashMap<String, Object>> getPropMap() {
        return propMap;
    }

    public Object getVid() {
        return vid;
    }

    public void setVid(Object vid) {
        this.vid = vid;
    }

    public void setPropMap(HashMap<String, HashMap<String, Object>> propMap) {
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
        Vertex vertex = (Vertex) o;
        return Objects.equals(vid, vertex.vid);
    }

    @Override
    public int hashCode() {
        return Objects.hash(vid, propMap);
    }

    public Graph graph() {
        return getGraph();
    }

    public List<String> getTags() {
        return null;
    }

    public boolean hasTag(String tagName) {
        return true;
    }

    /**
     * pass in tagName and attribute Map to add tag for vertex.
     *
     * @param name tagName
     * @param propMap attribute Map
     * @return whether add success
     */
    public boolean addTag(String name, HashMap<String, Object> propMap) {
        //update local
        //update db (insert vertex)
        return true;
    }

    /**
    * delete vertex.
    */
    public boolean clearAllTags() {
        getGraph().delete(this);
        return true;
    }

    /**
     * update propertyValue of tag.
     */
    public boolean updateTag(String name, HashMap<String, Object> propMap) {
        //judge vertex if exist in graph
        return true;
    }

    //(1 :QKM3{name: "asd", age: 19} :QKM2{name: "asd", age: 19})
    @Override
    public String toString() {
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
                    part.append(String.join(", ", prop)).append("}");
                }
            }
        }
        return String.format(result, vid, part);
    }
}
