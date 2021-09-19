/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.query.ngql;

import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.orm.entity.Graph;
import com.vesoft.nebula.orm.exception.ExecuteException;
import com.vesoft.nebula.orm.exception.InitException;
import com.vesoft.nebula.orm.operator.PathDirection;
import com.vesoft.nebula.orm.operator.PathType;
import com.vesoft.nebula.orm.query.cypher.Encoding;
import com.vesoft.nebula.orm.query.util.KeyWord;
import java.util.List;

/**
 * according to the list of start and destination points,
 * path type, edgeType and some result processing , such as order by、limit to get path.
 *
 * <p>query sentence is similar to 'FIND SHORTEST PATH FROM "player102" TO "team204" OVER *'.</p>
 * <p>the format returned is similar to (id)-[:edgeName@rank{}]->(id).</p>
 *
 * @author Qi Kai Meng
 */
public class FindPath extends NGqlQuery<FindPath> {
    private int steps = 5;
    private PathType pathType;
    private List<Object> srcIds;
    private List<Object> dstIds;
    private List<String> edges;
    private PathDirection pathDirection;
    private long limit = -1;
    private boolean isSort = false;
    private final Graph graph;

    public FindPath(Graph graph) {
        this.graph = graph;
    }


    /**
     * @param pathType SHORTEST or NOLOOP or ALL
     * @param srcIds   srcIds start idList
     * @param dstIds   end idList
     */
    protected FindPath init(PathType pathType, List<?> srcIds, List<?> dstIds, List<String> edges) {
        this.pathType = pathType;
        this.srcIds = (List<Object>) srcIds;
        this.dstIds = (List<Object>) dstIds;
        this.edges = edges;
        return this;
    }

    /**
     * pass in pathDirection.
     *
     * @param pathDirection from->to or to->from or both
     * @return FindPath
     */
    public FindPath pathDirection(PathDirection pathDirection) {
        this.pathDirection = pathDirection;
        return this;
    }

    /**
     * pass in steps,if you pass in negative,use default value
     *
     * @param steps relationship number,default value is five
     * @return FindPath
     */
    public FindPath steps(int steps) {
        this.steps = steps;
        return this;
    }

    /**
     * sort result paths by $-.PATH.
     *
     * @param isSort true or false,default value is false
     * @return FindPath
     */
    public FindPath orderBy(boolean isSort) {
        this.isSort = isSort;
        return this;
    }

    /**
     * return to previous limit lines.
     *
     * @param limit limit
     * @return FindPath
     */
    public FindPath limit(long limit) {
        this.limit = limit;
        return this;
    }


    private String connectParameters() {
        if (srcIds == null || srcIds.isEmpty()) {
            throw new InitException("srcIds can not be null");
        }
        if (dstIds == null || dstIds.isEmpty()) {
            throw new InitException("dstIds can not be null");
        }
        if (pathType == null) {
            throw new InitException("pathType can not be null");
        }
        StringBuilder result = new StringBuilder();
        result.append(KeyWord.FIND).append(" ").append(pathType).append(KeyWord.PATH).append(" ");
        result.append(String.format(KeyWord.FROM + " %s", Encoding.encodeIdList(srcIds)));
        result.append(String.format(" " + KeyWord.TO + " " + "%s ", Encoding.encodeIdList(dstIds)));
        if (edges == null || edges.isEmpty()) {
            result.append(KeyWord.OVER).append(" ").append(KeyWord.ALL).append(" ");
        } else {
            result.append(String.format(KeyWord.OVER + " %s ", String.join(",", edges)));
        }
        if (pathDirection != null) {
            result.append(pathDirection).append(" ");
        }
        if (steps >= 0) {
            result.append(KeyWord.UPTO).append(" ").append(steps).append(" ").append(KeyWord.STEPS);
        }
        if (isSort) {
            result.append(" ").append(KeyWord.PIPE).append(" ")
                .append(KeyWord.ORDER_BY).append(" ").append(KeyWord.$_PATH);
        }
        if (limit >= 0) {
            result.append(" ").append(KeyWord.PIPE).append(" ")
                .append(KeyWord.LIMIT).append(" ").append(limit);
        }
        return result.toString().trim();
    }

    /**
     * @return from result get the first
     */
    public ResultSet.Record first() {
        ResultSet all = all();
        if (!all.isEmpty()) {
            return all.rowValues(0);
        }
        return null;
    }

    /**
     * @return all qualified
     */
    public ResultSet all() {
        String query = connectParameters();
        ResultSet resultSet = graph.run(query);
        if (!resultSet.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }
        return resultSet;
    }

    /**
     * count of results.
     *
     * @return count
     */
    public long count() {
        ResultSet all = all();
        if (!all.isEmpty()) {
            return all.rowsSize();
        }
        return 0;
    }

    /**
     * is there data that meets the conditions.
     *
     * @return true or false
     */
    public boolean exist() {
        return !all().isEmpty();
    }
}
