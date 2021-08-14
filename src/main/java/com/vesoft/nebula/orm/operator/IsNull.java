/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.operator;

/**
 * used to do isNull operate,eg: name is null
 */
public class IsNull extends Condition {

    public IsNull() {
    }

    @Override
    protected String encode() {
        return "%s" + " is null ";
    }
}
