/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.query.ngql;

import com.vesoft.nebula.orm.entity.Graph;
import java.util.List;

/**
 * the user gets the {@link FetcherVertex} object by passing the {@link Graph} object
 * then you can fetchOne or fetch some.
 *
 * @author Qi Kai Meng
 */
public class FetcherVertex {
    private final FetchVertex fetchVertex;

    public FetcherVertex(Graph graph) {
        this.fetchVertex = new FetchVertex(graph);
    }

    public FetchVertex fetch(List<?> vidList) {
        return fetchVertex.init(vidList);
    }

    public FetchVertex fetchOne(Object id) {
        return fetchVertex.initOne(id);
    }
}
