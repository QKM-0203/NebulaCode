/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.exception;

/**
 * when inserting data, the data type passed did not match the type given.
 *
 * @author Qi Kai Meng
 */
public class DataTypeException extends RuntimeException {
    public DataTypeException() {

    }

    public DataTypeException(String message) {
        super(message);
    }
}
