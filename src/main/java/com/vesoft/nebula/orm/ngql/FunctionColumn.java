/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.ngql;

import com.vesoft.nebula.orm.operator.AggregateFunction;

/**
 * aliasing aggregate functions
 */
public class FunctionColumn {
    private AggregateFunction aggregateFunction;
    private String alias;

    public FunctionColumn(AggregateFunction aggregateFunction, String alias) {
        this.aggregateFunction = aggregateFunction;
        this.alias = alias;
    }

    public AggregateFunction getAggregateFunction() {
        return aggregateFunction;
    }

    public String getAlias() {
        return alias;
    }
}
