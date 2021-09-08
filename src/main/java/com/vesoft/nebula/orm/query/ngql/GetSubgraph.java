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
import com.vesoft.nebula.orm.operator.EdgeDirection;
import com.vesoft.nebula.orm.query.cypher.Encoding;
import com.vesoft.nebula.orm.query.cypher.Lexer;
import java.util.Arrays;
import java.util.List;

/**
 * getSubgraph by srcIds、edges、edgeDirection required.
 *
 * <p>query sentence 'GET SUBGRAPH 1 STEPS FROM "player100"'.</p>
 *
 * @author Qi Kai Meng
 */
public class GetSubgraph {
    private long steps = 1;
    private List<Object> srcIds;
    private EdgeDirection edgeDirection = EdgeDirection.BOTH;
    private List<String> edges;
    private final Graph graph;

    public GetSubgraph(Graph graph) {
        this.graph = graph;
    }

    protected GetSubgraph init(List<?> srcIds) {
        this.srcIds = (List<Object>) srcIds;
        return this;
    }

    /**
     * pass in edgeTypes,edgeDirection.
     *
     * @param edges         edgeTypes
     * @param edgeDirection out or in or all
     * @return FindPath
     */
    public GetSubgraph edges(EdgeDirection edgeDirection, String... edges) {
        this.edges = Arrays.asList(edges);
        this.edgeDirection = edgeDirection;
        return this;
    }

    /**
     * pass in steps,if you pass in negative,use default value 1.
     *
     * @param steps relationship number,default value is one
     * @return FindPath
     */
    public GetSubgraph steps(long steps) {
        this.steps = steps;
        return this;
    }

    private String connectParameters() {
        if (srcIds == null || srcIds.isEmpty()) {
            throw new InitException("srcIds can not be null");
        }
        StringBuilder result = new StringBuilder();
        result.append(Lexer.GET_SUBGRAPH);
        if (steps >= 0) {
            result.append(steps).append(Lexer.STEPS).append(" ");
        }
        result.append(String.format(Lexer.FROM + "%s ", Encoding.encodeIdList(srcIds)));
        if (edges != null && !edges.isEmpty()) {
            result.append(edgeDirection.toString()).append(" ").append(String.join(",", edges));
        }
        return result.toString().trim();
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
     * is there data that meets the conditions.
     *
     * @return true or false
     */
    public boolean exist() {
        return !all().isEmpty();
    }
}