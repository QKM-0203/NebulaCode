/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.operator;

/**
 * used to do list in operate,eg: "2" in ["1","2"]
 */
public class In extends Condition {
    public In(Object value) {
        super.value = value;
    }

    @Override
    protected String encode() {
        return "%s" + " IN " + value;
    }
}
