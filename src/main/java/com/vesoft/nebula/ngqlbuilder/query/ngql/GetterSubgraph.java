/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.ngqlbuilder.query.ngql;

import com.vesoft.nebula.ngqlbuilder.entity.Graph;
import java.util.List;

/**
 * the user gets the {@link GetterSubgraph} object by passing the {@link Graph} object
 * then you can call {@link #get(List)}.
 *
 * @author Qi Kai Meng
 */
public class GetterSubgraph {

    private final GetSubgraph getSubgraph;

    public GetterSubgraph(Graph graph) {
        this.getSubgraph = new GetSubgraph(graph);
    }

    public GetSubgraph get(List<?> srcIds) {
        return getSubgraph.init(srcIds);
    }
}
