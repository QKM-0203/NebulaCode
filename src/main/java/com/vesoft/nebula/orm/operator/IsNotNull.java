/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.operator;

/**
 * used to do isNotNull operate,eg: name is not null
 */
public class IsNotNull extends Condition {
    public IsNotNull() {
    }

    @Override
    protected String encode() {
        return "%s" + " is not null ";
    }
}
