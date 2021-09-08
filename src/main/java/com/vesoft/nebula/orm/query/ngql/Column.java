/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.query.ngql;

import com.vesoft.nebula.orm.operator.AggregateFunction;

/**
 * alias properties.
 *
 * @author Qi Kai Meng
 */
public class Column {
    private String propName;
    private AggregateFunction aggregateFunction;
    private String alias;


    public Column(String propName, String alias) {
        this.propName = propName;
        this.alias = alias;
    }

    public Column(AggregateFunction aggregateFunction, String alias) {
        this.aggregateFunction = aggregateFunction;
        this.alias = alias;
    }

    public String getPropName() {
        return propName;
    }

    public AggregateFunction getAggregateFunction() {
        return aggregateFunction;
    }

    public String getAlias() {
        return alias;
    }

    public void setPropName(String propName) {
        this.propName = propName;
    }

    public void setAggregateFunction(AggregateFunction aggregateFunction) {
        this.aggregateFunction = aggregateFunction;
    }

    public void setAlias(String alias) {
        this.alias = alias;
    }
}
