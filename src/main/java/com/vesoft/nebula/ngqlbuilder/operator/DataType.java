/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.ngqlbuilder.operator;

/**
 * the data type used to represent data.
 *
 * <p>users pass in data types by enumerating instances.</p>
 *
 * @author Qi Kai Meng
 */
public enum DataType {
    DATE(1),
    DATETIME(2),
    DOUBLE(3),
    FLOAT(4),
    INT8(5),
    INT16(6),
    INT(7),
    FIXED_STRING(8),
    INT64(9),
    STRING(10),
    TIME(11),
    //pass in long
    TIMESTAMP(12),
    BOOL(13);

    int length = 0;

    DataType(int length) {
        this.length = length;
    }

    public int getLength() {
        return length;
    }

    public DataType setLength(int length) {
        this.length = length;
        return this;
    }
}
