/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.ngqlbuilder.operator;

/**
 * {@code PathType} is used to show pathType,
 * SHORTEST、ALL、NOLOOP represents the shortest path, find all paths
 * and find acyclic paths respectively.
 *
 * @author Qi Kai Meng
 */
public enum PathType {
    SHORTEST,
    ALL,
    NOLOOP
}
