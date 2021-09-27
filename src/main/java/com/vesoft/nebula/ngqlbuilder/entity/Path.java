/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.ngqlbuilder.entity;

import com.vesoft.nebula.ngqlbuilder.exception.ExecuteException;
import com.vesoft.nebula.ngqlbuilder.exception.InitException;
import java.util.*;

/**
 * users can create a path by passing in List<{@link Segment}> to insert data,
 * and can also accept the post query path.
 *
 * <p>after the user passes in a group of paths,
 * first call the init method to initialize the path to determine whether it is a legal path.
 * In the judgment process, the traversal order of the path is directly stored in the
 * {@link #sequence}.Finally, the vertex and edges are stored separately according to the sequence
 * and passed to the parent class {@link Walkable}.</p>
 *
 * <p>The user can obtain the end node on the path
 * according to {@link Walkable#getEndVertex()} and the start node according to
 * {@link Walkable#getStartVertex()}.
 * The user can call {@link #walk()} to traverse the path and output the form of points and edges.
 * </p>
 *
 * @author Qi Kai Meng
 */
public class Path extends Walkable {
    private List<Object> sequence = new ArrayList<>();
    private List<Segment> segments;

    public Path(List<Segment> path) {
        this.segments = path;
        init(path);
    }

    /**
     * judge whether it is a path. If so, store the traversal order in the {@link #sequence}
     * at the same time.
     *
     * @param path path information
     */
    public void init(List<Segment> path) {
        if (path == null) {
            throw new NullPointerException("path object is null");
        }
        sequence.add(path.get(0).getStartVertex());
        for (Segment segment : path) {
            Vertex lastVertex = (Vertex) sequence.get(sequence.size() - 1);
            if (lastVertex != null && segment != null
                && segment.getStartVertex() != null
                && lastVertex.getVid().equals(segment.getStartVertex().getVid())) {
                if (segment.getRelationship() != null
                    && lastVertex.getVid().equals(segment.getRelationship().getStartVid())) {
                    sequence.add(segment.getRelationship());
                    if (segment.getEndVertex() != null
                        && segment.getRelationship().getEndVid()
                        .equals(segment.getEndVertex().getVid())) {
                        sequence.add(segment.getEndVertex());
                    } else if (segment.getEndVertex() == null) {
                        sequence.add(null);
                    } else {
                        sequence.clear();
                        throw new ExecuteException(String.format("%s can not connect %s",
                            segment.getRelationship(), segment.getEndVertex()));
                    }
                } else if (segment.getRelationship() != null
                    && lastVertex.getVid().equals(segment.getRelationship().getEndVid())) {
                    sequence.add(segment.getRelationship());
                    if (segment.getEndVertex() != null
                        && segment.getRelationship().getStartVid()
                        .equals(segment.getEndVertex().getVid())) {
                        sequence.add(segment.getEndVertex());
                    } else if (segment.getEndVertex() == null) {
                        sequence.add(null);
                    } else {
                        sequence.clear();
                        throw new ExecuteException(String.format("%s can not connect %s",
                            segment.getRelationship(), segment.getEndVertex()));
                    }
                } else if (segment.getRelationship() == null) {
                    if (segment.getEndVertex() == null) {
                        Relationship relationship = new Relationship(segment.getStartVertex()
                            .getVid(), segment.getStartVertex().getVid(), "Relationship");
                        segment.setRelationship(relationship);
                        segment.setEndVertex(segment.getStartVertex());
                    } else {
                        Relationship relationship = new Relationship(segment.getStartVertex()
                            .getVid(), segment.getEndVertex().getVid(), "Relationship");
                        segment.setRelationship(relationship);
                    }
                    sequence.add(segment.getRelationship());
                    sequence.add(segment.getEndVertex());
                } else {
                    sequence.clear();
                    throw new ExecuteException(String.format("%s can not connect %s",
                        lastVertex, segment.getRelationship()));
                }
            } else if (lastVertex != null && segment != null
                && segment.getEndVertex() != null
                && lastVertex.getVid().equals(segment.getEndVertex().getVid())) {
                if (segment.getRelationship() != null
                    && lastVertex.getVid().equals(segment.getRelationship().getStartVid())) {
                    sequence.add(segment.getRelationship());
                    if (segment.getStartVertex().getVid() != null
                        && segment.getRelationship().getEndVid()
                        .equals(segment.getStartVertex().getVid())) {
                        sequence.add(segment.getStartVertex());
                    } else if (segment.getStartVertex().getVid() == null) {
                        sequence.add(null);
                    } else {
                        sequence.clear();
                        throw new ExecuteException(String.format("%s can not connect %s",
                            segment.getRelationship(), segment.getStartVertex()));
                    }
                } else if (segment.getRelationship() != null
                    && lastVertex.getVid().equals(segment.getRelationship().getEndVid())) {
                    sequence.add(segment.getRelationship());
                    if (segment.getStartVertex() != null
                        && segment.getRelationship().getStartVid()
                        .equals(segment.getStartVertex().getVid())) {
                        sequence.add(segment.getStartVertex());
                    } else if (segment.getStartVertex() == null) {
                        sequence.add(null);
                    } else {
                        sequence.clear();
                        throw new ExecuteException(String.format("%s can not connect %s",
                            segment.getRelationship(), segment.getStartVertex()));
                    }
                } else if (segment.getRelationship() == null) {
                    if (segment.getEndVertex() == null) {
                        Relationship relationship = new Relationship(segment.getStartVertex()
                            .getVid(), segment.getStartVertex().getVid(), "Relationship");
                        segment.setRelationship(relationship);
                        segment.setEndVertex(segment.getStartVertex());
                    } else {
                        Relationship relationship = new Relationship(segment.getStartVertex()
                            .getVid(), segment.getEndVertex().getVid(), "Relationship");
                        segment.setRelationship(relationship);
                    }
                    sequence.add(segment.getRelationship());
                    sequence.add(segment.getEndVertex());
                } else {
                    sequence.clear();
                    throw new ExecuteException(String.format("%s can not connect %s",
                        lastVertex, segment.getRelationship()));
                }
            } else {
                sequence.clear();
                throw new ExecuteException(String.format("%s can not connect %s",
                    lastVertex, segment));
            }
        }
        ArrayList<Vertex> vertices = new ArrayList<>();
        ArrayList<Relationship> relationships = new ArrayList<>();
        for (Object o : sequence) {
            if (o instanceof Vertex) {
                vertices.add((Vertex) o);
            } else if (o instanceof Relationship) {
                relationships.add((Relationship) o);
            }
        }
        super.init(relationships, vertices);
    }

    /**
     * traversal {@link #sequence} output.
     */
    public List<Object> walk() {
        if (sequence.isEmpty()) {
            throw new InitException("path cannot be traversed");
        }
        return sequence;
    }

    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();
        for (int index = 0; index < sequence.size(); index++) {
            if (sequence.get(index) instanceof Vertex) {
                result.append(sequence.get(index));
            } else {
                ArrayList<String> prop = new ArrayList<>();
                Map<String, Object> propMap = ((Relationship) sequence.get(index)).getPropMap();
                StringBuilder propValue = new StringBuilder();
                if (propMap != null && !propMap.isEmpty()) {
                    for (String propName : ((Relationship)
                        sequence.get(index)).getPropMap().keySet()) {
                        if (propMap.get(propName) instanceof String) {
                            prop.add(String.format("%s: \"%s\"", propName, propMap.get(propName)));
                        } else {
                            prop.add(String.format("%s: %s", propName, propMap.get(propName)));
                        }
                    }
                    propValue.append(String.join(", ", prop));
                }
                if (((Relationship) sequence.get(index)).getStartVid().equals(
                    ((Vertex) sequence.get(index - 1)).getVid())) {
                    result.append(String.format("-[:%s@%s{%s}]->",
                        ((Relationship) sequence.get(index)).getEdgeName(),
                        ((Relationship) sequence.get(index)).getRank(),
                        propValue));
                } else {
                    result.append(String.format("<-[:%s@%s{%s}]-",
                        ((Relationship) sequence.get(index)).getEdgeName(),
                        ((Relationship) sequence.get(index)).getRank(),
                        propValue));
                }
            }
        }
        return String.format("<%s>", result);
    }

    public List<Segment> getSegments() {
        return segments;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        Path path = (Path) o;
        return Objects.equals(segments, path.segments);
    }

    @Override
    public int hashCode() {
        return Objects.hash(segments);
    }
}
