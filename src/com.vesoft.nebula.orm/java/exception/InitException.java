/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package exception;

/**
 * init data raise exception,eg:path、subgraph.
 *
 * @author Qi Kai Meng
 */
public class InitException extends ExecuteException {
    public InitException() {

    }

    public InitException(String message) {
        super(message);
    }
}
