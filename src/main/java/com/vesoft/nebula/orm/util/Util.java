/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.util;

import com.vesoft.nebula.client.graph.data.DateTimeWrapper;
import com.vesoft.nebula.client.graph.data.TimeWrapper;
import com.vesoft.nebula.client.graph.data.ValueWrapper;
import com.vesoft.nebula.orm.datatype.DateTime;
import com.vesoft.nebula.orm.datatype.Time;
import com.vesoft.nebula.orm.entity.*;
import com.vesoft.nebula.orm.exception.ExecuteException;
import com.vesoft.nebula.orm.ngql.Encoding;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * this class is a tool class
 *
 * @author Qi Kai Meng
 */
public class Util {
    /**
     * classify Vertex use tag.
     *
     * @param vertexList many vertexes
     * @return classify
     */
    public static HashMap<String, List<Vertex>> joinSameTagVertices(List<Vertex> vertexList) {
        HashMap<String, List<Vertex>> classifyVertex = new HashMap<>();
        for (Vertex vertex : vertexList) {
            String tagJoin = Encoding.joinTag(vertex.getPropMap());
            if (classifyVertex.get(tagJoin) == null) {
                ArrayList<Vertex> vertices = new ArrayList<>();
                vertices.add(vertex);
                classifyVertex.put(tagJoin, vertices);
            } else {
                List<Vertex> vertices = classifyVertex.get(tagJoin);
                vertices.add(vertex);
                classifyVertex.put(tagJoin, vertices);
            }
        }
        return classifyVertex;
    }

    /**
     * classify edge use edge.
     *
     * @param relationshipList many relationships
     * @param flag             is it classified by edgeName or by edgeName and attributes name,
     *                         flag = 0 symbol by edgeName and attributes name
     * @return classify
     */
    public static HashMap<String, List<Relationship>> joinSameEdgeRelationships(
        List<Relationship> relationshipList, int flag) {
        HashMap<String, List<Relationship>> classifyEdge = new HashMap<>();
        if (flag == 0) {
            for (Relationship relationship : relationshipList) {
                String edge;
                if (relationship.getPropMap() == null || relationship.getPropMap().size() == 0) {
                    edge = Encoding.joinEdge(relationship.getEdgeName(), null);
                } else {
                    edge = Encoding.joinEdge(relationship.getEdgeName(),
                        relationship.getPropMap().keySet());
                }
                if (classifyEdge.get(edge) == null) {
                    ArrayList<Relationship> relationships = new ArrayList<>();
                    relationships.add(relationship);
                    classifyEdge.put(edge, relationships);
                } else {
                    List<Relationship> relationships = classifyEdge.get(edge);
                    relationships.add(relationship);
                }
            }
        } else {
            for (Relationship relationship : relationshipList) {
                String edgeName = relationship.getEdgeName();
                if (classifyEdge.get(edgeName) == null) {
                    ArrayList<Relationship> relationships = new ArrayList<>();
                    relationships.add(relationship);
                    classifyEdge.put(edgeName, relationships);
                } else {
                    List<Relationship> relationships = classifyEdge.get(edgeName);
                    relationships.add(relationship);
                }
            }
        }

        return classifyEdge;
    }

    /**
     * determine which graph object it is,store vertex and edges separately
     *
     * @param vertices      collection of storage vertex
     * @param relationships collection of storage relationships
     * @param graphObject   graphObject of space
     */
    public static void judgeGraphObject(ArrayList<Vertex> vertices,
                                        ArrayList<Relationship> relationships, Object graphObject) {
        if (graphObject instanceof Vertex) {
            vertices.add((Vertex) graphObject);
        } else if (graphObject instanceof Relationship) {
            relationships.add((Relationship) graphObject);
        } else if (graphObject instanceof Subgraph) {
            vertices.addAll(((Subgraph) graphObject).getVertexes());
            relationships.addAll(((Subgraph) graphObject).getRelationships());
        } else if (graphObject instanceof Path) {
            vertices.addAll(((Path) graphObject).getVertices());
            relationships.addAll(((Path) graphObject).getRelationships());
        } else {
            throw new ExecuteException(String.format("%s object is not support",
                graphObject.getClass().getName()));
        }
    }

    /**
     * the valueWrapper type obtained by executing the run method
     * needs to determine the actual type and convert it
     *
     * @param propMap         convert it and save
     * @param valueWrapperMap value of execute run method
     * @throws UnsupportedEncodingException convert it arise question
     */
    public static void judgeValueWrapper(HashMap<String, Object> propMap,
                                         HashMap<String, ValueWrapper> valueWrapperMap)
        throws UnsupportedEncodingException {
        for (String propName : valueWrapperMap.keySet()) {
            if (valueWrapperMap.get(propName).isString()) {
                propMap.put(propName, valueWrapperMap.get(propName).asString());
            } else if (valueWrapperMap.get(propName).isDate()) {
                propMap.put(propName, valueWrapperMap.get(propName).asDate());
            } else if (valueWrapperMap.get(propName).isBoolean()) {
                propMap.put(propName, valueWrapperMap.get(propName).asBoolean());
            } else if (valueWrapperMap.get(propName).isDateTime()) {
                DateTimeWrapper dateTimeWrapper = valueWrapperMap.get(propName).asDateTime();
                DateTime dateTime = new DateTime(dateTimeWrapper.getLocalDateTimeStr());
                dateTime.setDateTimeWrapper(dateTimeWrapper);
                propMap.put(propName, dateTime);
            } else if (valueWrapperMap.get(propName).isTime()) {
                TimeWrapper timeWrapper = valueWrapperMap.get(propName).asTime();
                Time time = new Time(timeWrapper.getLocalTimeStr());
                time.setTimeWrapper(timeWrapper);
                propMap.put(propName, time);
            } else if (valueWrapperMap.get(propName).isDouble()) {
                propMap.put(propName, valueWrapperMap.get(propName).asDouble());
            } else if (valueWrapperMap.get(propName).isLong()) {
                propMap.put(propName, valueWrapperMap.get(propName).asLong());
            } else {
                propMap.put(propName, valueWrapperMap.get(propName).asNull());
            }
        }
    }
}
