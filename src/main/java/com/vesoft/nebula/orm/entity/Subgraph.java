/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.entity;

import com.vesoft.nebula.orm.exception.InitException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * a subgraph is a set of points and edges.
 *
 * @author Qi Kai Meng
 */
public class Subgraph {
    private List<Vertex> vertexList;
    private List<Relationship> relationshipsList;

    public Subgraph(List<Vertex> vertexList, List<Relationship> relationshipsList) {
        init(vertexList, relationshipsList);
    }

    public Subgraph(List<Vertex> vertexList) {
        init(vertexList, null);
    }

    protected void init(List<Vertex> vertexList, List<Relationship> relationshipsList) {
        this.vertexList = vertexList;
        this.relationshipsList = relationshipsList;
        if (vertexList == null || vertexList.isEmpty()) {
            throw new InitException("the subgraph contains at least one point");
        }
    }

    /**
     * get all vertexes from subgraph.
     *
     * @return collection of all vertexes
     */
    public List<Vertex> getVertexes() {
        if (vertexList == null || vertexList.isEmpty()) {
            return null;
        }
        return vertexList;
    }

    /**
     * get all relationship from subgraph.
     *
     * @return collection of all relationships
     */
    public List<Relationship> getRelationships() {
        if (relationshipsList == null || relationshipsList.isEmpty()) {
            return null;
        }
        return relationshipsList;
    }

    /**
     * gets the graph space object bound by the subgraph.
     *
     * @return graph space object
     */
    public Graph graph() {
        if ((vertexList == null || vertexList.isEmpty())
            && (relationshipsList == null || relationshipsList.isEmpty())) {
            return null;
        } else {
            if (vertexList != null && !vertexList.isEmpty()) {
                return vertexList.get(0).getGraph();
            } else {
                return relationshipsList.get(0).getGraph();
            }
        }
    }

    /**
     * include all tagNames at all vertexes and is de weighted.
     *
     * <p>if user want to get the latest,please use {@link Graph#pull} from remote get latest data
     * </p>
     *
     * @return Set
     */
    public Set<String> tags() {
        Set<String> tagNames = new HashSet<>();
        for (Vertex vertex : vertexList) {
            tagNames.addAll(vertex.getTagNames());
        }
        return tagNames;
    }

    /**
     * include all edgeNames at all relationships and is de weighted.
     *
     * <p>if user want to get the latest,please use {@link Graph#pull} from remote get latest data
     * </p>
     *
     * @return Set
     */
    public Set<String> types() {
        Set<String> edgeTypes = new HashSet<>();
        for (Relationship relationship : relationshipsList) {
            edgeTypes.add(relationship.getEdgeName());
        }
        return edgeTypes;
    }

    /**
     * get attribute name List from tag and edge and is de weighted.
     *
     * <p>if user want to get the latest,please use {@link Graph#pull} from remote get latest data
     * </p>
     *
     * @return attribute name List ,is de weighted
     */
    public Set<String> properties() {
        Set<String> properties = new HashSet<>();
        for (Vertex vertex : vertexList) {
            for (String tagName : vertex.getTagNames()) {
                properties.addAll(vertex.getTag(tagName).keySet());
            }
        }
        for (Relationship relationship : relationshipsList) {
            properties.addAll(relationship.getPropMap().keySet());
        }
        return properties;
    }

    @Override
    public String toString() {
        return "Subgraph{"
            + "vertex" + vertexList
            + ", relationships" + relationshipsList
            + '}';
    }
}
