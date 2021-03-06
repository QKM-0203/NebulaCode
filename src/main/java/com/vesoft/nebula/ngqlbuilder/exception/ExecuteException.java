/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.ngqlbuilder.exception;

/**
 * execute nGql sentence raise an exception.
 *
 * @author Qi Kai Meng
 */
public class ExecuteException extends RuntimeException {

    public ExecuteException() {

    }

    public ExecuteException(String message) {
        super(message);
    }
}
