/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.entity;

import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.orm.exception.ExecuteException;
import com.vesoft.nebula.orm.exception.InitException;
import com.vesoft.nebula.orm.ngql.Encoding;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Objects;
import javax.naming.NameNotFoundException;

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
 * @author Qi Kai Meng
 *
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

    public ResultSet getTags() {
        return getGraph().run("FETCH PROP ON * " + (this.getVid() instanceof String
            ? "\"" + this.getVid() + "\"" : this.getVid()));
    }

    public boolean hasTag(String tagName) {
        ResultSet run = getGraph().run("FETCH PROP ON " + tagName + (this.getVid() instanceof String
            ? "\"" + this.getVid() + "\"" : this.getVid()));
        return run.getRows().size() != 0;
    }

    /**
     * pass in tagName and attribute Map to add tag for vertex.
     *
     * @param name tagName
     * @param propMap attribute Map
     */
    public void addTag(String name, HashMap<String, Object> propMap) {
        HashMap<String, HashMap<String, Object>> tagMap = this.getPropMap();
        tagMap.put(name, propMap);
        getGraph().create(this);
    }

    @Override
    public Graph getGraph() {
        return super.getGraph();
    }

    /**
     * delete vertex.
     */
    public void clearAllTags() {
        getGraph().delete(this);
    }

    /**
     * update propertyValue of tag.
     */
    public void updateTag(String name, HashMap<String, Object> propMap) throws
        NameNotFoundException {
        boolean hasTag = hasTag(name);
        if (hasTag) {
            ResultSet resultSet = getGraph().run("UPDATE VERTEX ON " + "`" + name + "`" + (this.getVid() instanceof String
                ? "\"" + this.getVid() + "\"" : this.getVid()) + "SET "
                + Encoding.updateSchemaValue(propMap));
            if (resultSet.isSucceeded()) {
                this.getPropMap().put(name, propMap);
            } else {
                throw new ExecuteException(resultSet.getErrorMessage());
            }
        } else {
            throw new NameNotFoundException("tag" + name + "is not found");
        }
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