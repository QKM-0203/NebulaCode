/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package entity;

/**
 * a set of vertex and edge vertex forms of {@link Path} like.
 *
 * @author Qi Kai Meng
 */
public class Part {
    private Vertex startVertex;
    private Vertex endVertex;
    private Relationship relationship;

    /**
     * you can pass in startVertex and edges and endVertex to construct part of the path.
     *
     * @param startVertex  startVertex
     * @param relationship relationship
     * @param endVertex endVertex
     */
    public Part(Vertex startVertex, Relationship relationship, Vertex endVertex) {
        this.startVertex = startVertex;
        this.endVertex = endVertex;
        this.relationship = relationship;
    }

    public Vertex getStartVertex() {
        return startVertex;
    }

    public Vertex getEndVertex() {
        return endVertex;
    }

    public Relationship getRelationship() {
        return relationship;
    }
}
