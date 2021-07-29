/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package entity;

import java.util.List;

/**
 * is a traversable object,can traverse traversable subgraph and paths.
 *
 * <p>you can get the start node, end node, and step size</p>
 *
 * @author Qi Kai Meng
 */
public class Walkable {
    private List<Relationship> relationships;
    private List<Vertex> vertices;

    protected void init(List<Relationship> relationships, List<Vertex> vertices) {
        this.relationships = relationships;
        this.vertices = vertices;
    }

    /**
     * get startVertex from vertexList.
     *
     * @return startVertex
     */
    public Vertex getStartVertex() {
        if (vertices == null || vertices.isEmpty()) {
            return null;
        }
        return vertices.get(0);
    }

    /**
     * get endVertex from vertexList.
     *
     * @return endVertex
     */
    public Vertex getEndVertex() {
        if (vertices == null || vertices.isEmpty()) {
            return null;
        }
        return vertices.get(vertices.size() - 1);
    }

    /**
     * get all vertices.
     */
    public List<Vertex> getVertices() {
        if (vertices == null || vertices.isEmpty()) {
            return null;
        }
        return vertices;
    }

    /**
     * get all relationships.
     */
    public List<Relationship> getRelationships() {
        if (relationships == null || relationships.isEmpty()) {
            return null;
        }
        return relationships;
    }

    /**
     * get size of relationships.
     *
     * @return relationships size
     */
    public int steps() {
        if (relationships == null || relationships.isEmpty()) {
            return 0;
        }
        return relationships.size();
    }
}
