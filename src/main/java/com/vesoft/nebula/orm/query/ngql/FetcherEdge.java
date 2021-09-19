/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.query.ngql;

import com.vesoft.nebula.orm.entity.Graph;
import com.vesoft.nebula.orm.entity.Relationship;
import java.util.List;

/**
 * the user gets the {@link FetcherEdge} object by passing the {@link Graph} object
 * then you can fetchOne or fetch some.
 *
 * @author Qi Kai Meng
 */
public class FetcherEdge {
    private final FetchEdge fetchEdge;

    public FetcherEdge(Graph graph) {
        this.fetchEdge = new FetchEdge(graph);
    }

    public FetchEdge fetchEdge(List<Relationship> relationships) {
        return fetchEdge.init(relationships);
    }

    public FetchEdge fetchOneEdge(Relationship relationship) {
        return fetchEdge.init(relationship);
    }
}
