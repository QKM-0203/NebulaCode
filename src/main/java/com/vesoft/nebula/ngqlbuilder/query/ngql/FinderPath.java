/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.ngqlbuilder.query.ngql;

import com.vesoft.nebula.ngqlbuilder.entity.Graph;
import com.vesoft.nebula.ngqlbuilder.operator.PathType;
import java.util.List;

/**
 * the user gets the {@link FinderPath} object by passing the {@link Graph} object
 * then you can call {@link #find(PathType, List, List, List)}.
 *
 * @author Qi Kai Meng
 */
public class FinderPath {
    private final FindPath findPath;

    public FinderPath(Graph graph) {
        this.findPath = new FindPath(graph);
    }

    public FindPath find(PathType pathType, List<?> srcIds, List<?> dstIds, List<String> edges) {
        return findPath.init(pathType, srcIds, dstIds, edges);
    }
}
