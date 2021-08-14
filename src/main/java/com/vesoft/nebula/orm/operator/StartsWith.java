/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.operator;

/**
 * used to do startWith matching operate,eg: name starts With 'q'
 */
public class StartsWith extends Condition {
    public StartsWith(Object value) {
        super.value = value;
    }

    @Override
    protected String encode() {
        return "%s" + " STARTS WITH " + value;
    }
}
