/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.operator;

/**
 * used to do regular matching operate,eg: name =~ "qkm.*"
 */
public class Regular extends Condition {
    public Regular(Object value) {
        this.value = value;
    }

    @Override
    protected String encode() {
        return "%s" + " =~ " + value;
    }
}
