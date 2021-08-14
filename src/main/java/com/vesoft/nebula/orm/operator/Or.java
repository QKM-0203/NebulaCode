/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.operator;

/**
 * used to do or operate,eg: name == "qkm" or name == "sc"
 */
public class Or extends Condition {
    public Or(Condition value1, Condition value2) {
        super.value1 = value1;
        super.value2 = value2;
    }

    @Override
    protected String encode() {
        return value1.encode() + " or " + value2.encode();
    }
}
