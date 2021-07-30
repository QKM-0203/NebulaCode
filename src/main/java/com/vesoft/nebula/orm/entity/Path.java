/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.entity;

import com.vesoft.nebula.orm.exception.ExecuteException;
import com.vesoft.nebula.orm.exception.InitException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * users can create a path by passing in List<{@link Part}> to insert data,
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
 * The user can call {@link #walk()} to traverse the path and output the form of points and edges
 * </p>
 *
 * @author Qi Kai Meng
 */
public class Path extends Walkable {
    private List<Object> sequence = new ArrayList<>();

    public Path(List<Part> path) {
        init(path);
    }


    /**
     * judge whether it is a path. If so, store the traversal order in the {@link #sequence}
     * at the same time.
     *
     * @param path path information
     */
    public void init(List<Part> path) {
        if (path == null) {
            throw new NullPointerException("path object is null");
        }
        sequence.add(path.get(0).getStartVertex());
        for (Part part : path) {
            Vertex lastVertex = (Vertex) sequence.get(sequence.size() - 1);
            if (lastVertex.getVid().equals(part.getStartVertex().getVid())) {
                if (lastVertex.getVid().equals(part.getRelationship().getStartVid())) {
                    sequence.add(part.getRelationship());
                    if (part.getEndVertex().getVid().equals(part.getRelationship().getEndVid())) {
                        sequence.add(part.getEndVertex());
                    } else {
                        sequence.clear();
                        throw new ExecuteException(String.format("%s can not connect %s",
                            part.getRelationship().getEndVid(), part.getEndVertex()));

                    }
                } else if (lastVertex.getVid().equals(part.getRelationship().getEndVid())) {
                    sequence.add(part.getRelationship());
                    if (part.getEndVertex().getVid().equals(part.getRelationship().getStartVid())) {
                        sequence.add(part.getEndVertex());
                    } else {
                        sequence.clear();
                        throw new ExecuteException(String.format("%s can not connect %s",
                            part.getRelationship().getStartVid(), part.getEndVertex()));
                    }
                } else {
                    sequence.clear();
                    throw new ExecuteException(String.format("%s can not connect %s",
                        lastVertex, part.getStartVertex()));
                }
            } else {
                sequence.clear();
                throw new ExecuteException(String.format("%s can not connect %s",
                    lastVertex, part.getStartVertex()));
            }
        }
        ArrayList<Vertex> vertices = new ArrayList<>();
        ArrayList<Relationship> relationships = new ArrayList<>();
        for (Object o : sequence) {
            if (o instanceof Vertex) {
                vertices.add((Vertex) o);
            } else {
                relationships.add((Relationship) o);
            }
        }
        super.init(relationships, vertices);
    }

    /**
     * traversal {@link #sequence} output.
     */
    public void walk() {
        if (sequence.isEmpty()) {
            throw new InitException("path cannot be traversed");
        }
        for (Object o : sequence) {
            System.out.println(o);
        }
    }

    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();
        for (int index = 0; index < sequence.size(); index++) {
            if (sequence.get(index) instanceof Vertex) {
                result.append((Vertex) sequence.get(index));
            } else {
                ArrayList<String> prop = new ArrayList<>();
                HashMap<String, Object> propMap = ((Relationship) sequence.get(index)).getPropMap();
                StringBuilder propValue = new StringBuilder();
                if (propMap == null || propMap.isEmpty()) {
                    propValue.append("");
                } else {
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
}
