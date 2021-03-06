/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.ngqlbuilder.entity;

import java.util.Objects;

/**
 * a set of vertex and edge vertex forms of {@link Path} like.
 *
 * @author Qi Kai Meng
 */
public class Segment {
    private Vertex startVertex;
    private Vertex endVertex;
    private Relationship relationship;

    /**
     * you can pass in startVertex and edges and endVertex to construct part of the path.
     *
     * @param startVertex  startVertex
     * @param relationship relationship
     * @param endVertex    endVertex
     */
    public Segment(Vertex startVertex, Relationship relationship, Vertex endVertex) {
        this.startVertex = startVertex;
        this.endVertex = endVertex;
        this.relationship = relationship;
        init(startVertex, relationship, endVertex);
    }

    private void init(Vertex startVertex, Relationship relationship, Vertex endVertex) {

    }

    public void setStartVertex(Vertex startVertex) {
        this.startVertex = startVertex;
    }

    public void setEndVertex(Vertex endVertex) {
        this.endVertex = endVertex;
    }

    public void setRelationship(Relationship relationship) {
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

    @Override
    public String toString() {
        return "Segment{"
            + "startVertex=" + startVertex
            + ", endVertex=" + endVertex
            + ", relationship=" + relationship
            + '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        Segment segment = (Segment) o;
        return Objects.equals(startVertex, segment.startVertex)
            && Objects.equals(endVertex, segment.endVertex)
            && Objects.equals(relationship, segment.relationship);
    }

    @Override
    public int hashCode() {
        return Objects.hash(startVertex, endVertex, relationship);
    }
}