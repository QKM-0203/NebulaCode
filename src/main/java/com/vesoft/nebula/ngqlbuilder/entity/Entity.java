/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.ngqlbuilder.entity;

/**
 * {@code Entity} is used to bind points, edges, subgraph and corresponding
 * graph spaces.
 *
 * @author Qi Kai Meng
 */
public class Entity {
    private Graph graph;

    /**
     * Bind the corresponding graph space object.
     *
     * @param graph graph space object
     */
    public void setGraph(Graph graph) {
        this.graph = graph;
    }

    public Graph getGraph() {
        return graph;
    }
}
