/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.ngqlbuilder.query.ngql;

import com.vesoft.nebula.ngqlbuilder.entity.Graph;
import java.util.List;

/**
 * the user gets the {@link Goer} object by passing the {@link Graph} object,
 * and then calls the {@link #go(List, List)} method to pass the parameters.
 *
 * @author Qi Kai Meng
 */
public class Goer {
    private final Go go;

    public Goer(Graph graph) {
        this.go = new Go(graph);
    }

    public Go go(List<?> srcIds, List<String> edges) {
        return go.init(srcIds, edges);
    }
}
