/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */
package common;

import entity.Graph;

public  interface Operate<T>  {
    void create(T Entity) throws NoSuchFieldException, IllegalAccessException;
    void drop(T Entity) throws NoSuchFieldException, IllegalAccessException;
    private int get(){
        return 1;
    }
}
