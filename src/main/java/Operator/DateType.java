/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */
package Operator;



public enum DateType {
    DATE(1),
    DATETIME(2),
    DOUBLE(3),
    FLOAT(4),
    INT8(5),
    INT16(6),
    INT32(7),
    FIXED_STRING(8),
    INT64(9),
    STRING(10),
    TIME(11),
    TIMESTAMP(12),
    BOOL(13);

    int length = 0;

    DateType(int length){
        this.length = length;
    }


    public int getLength() {
        return length;
    }

    public DateType setLength(int length) {
        this.length = length;
        return this;
    }


    public static DateType findByValue(int value) {
        switch (value) {
            case 1:
                return DATE;
            case 2:
                return DATETIME;
            case 3:
                return DOUBLE;
            case 4:
                return FLOAT;
            case 5:
                return INT8;
            case 6:
                return INT16;
            case 7:
                return INT32;
            case 8:
                return FIXED_STRING;
            case 9:
                return INT64;
            case 10:
                return STRING;
            case 11:
                return TIME;
            case 12:
                return TIMESTAMP;
            case 13:
                return BOOL;
            default:
                return null;
        }
    }
}
