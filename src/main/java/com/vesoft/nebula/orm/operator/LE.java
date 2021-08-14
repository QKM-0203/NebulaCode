/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.operator;

/**
 * used to do <= operate,eg: age <= 19
 */
public class LE extends Condition {
    public LE(Object value) {
        super.value = value;
    }

    @Override
    protected String encode() {
        return "%s" + " <= " + value;
    }
}
