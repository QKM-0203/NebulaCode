/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.query.ngql;

import com.vesoft.nebula.orm.entity.Graph;

/**
 * the user gets the {@link LookerUp} object by passing the {@link Graph} object,
 * and then calls the {@link #lookUp(String)} method to pass the parameters.
 *
 * @author Qi Kai Meng
 */
public class LookerUp {

    private final LookUp lookUp;

    public LookerUp(Graph graph) {
        this.lookUp = new LookUp(graph);
    }

    public LookUp lookUp(String schema) {
        return lookUp.init(schema);
    }

}
