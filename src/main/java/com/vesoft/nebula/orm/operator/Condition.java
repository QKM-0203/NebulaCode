/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.operator;

/**
 * parent class of all operations
 */
public class Condition {
    protected Object value;
    protected Condition value1;
    protected Condition value2;

    protected String encode() {
        return "";
    }
}



