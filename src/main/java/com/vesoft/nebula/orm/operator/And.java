/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.operator;

/**
 * used to do and operate,eg: name == "qkm" and age == 18
 */
public class And extends Condition {

    public And(Condition value1, Condition value2) {
        super.value1 = value1;
        super.value2 = value2;
    }

    @Override
    protected String encode() {
        return value1.encode() + " and " + value2.encode();
    }
}
