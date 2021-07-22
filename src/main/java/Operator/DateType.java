/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */
package Operator;


public enum DateType {
    DATE,
    DATETIME,
    DOUBLE,
    FIXED_STRING(8),
    FLOAT,
    INT8,
    INT16,
    INT32,
    INT64,
    STRING,
    TIME,
    TIMESTAMP,
    BOOL;
    int length = 0;
    DateType(int length){
        this.length = length;
    }
    DateType(){

    }

    public int getLength() {
        return length;
    }

    public DateType setLength(int length) {
        this.length = length;
        return this;
    }
}
