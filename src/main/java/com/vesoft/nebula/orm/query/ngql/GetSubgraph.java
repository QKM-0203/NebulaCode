/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.query.ngql;

import com.vesoft.nebula.orm.entity.Graph;
import com.vesoft.nebula.orm.exception.InitException;
import com.vesoft.nebula.orm.operator.EdgeDirection;
import com.vesoft.nebula.orm.query.cypher.Encoding;
import com.vesoft.nebula.orm.query.util.KeyWord;
import java.util.Arrays;
import java.util.List;

/**
 * getSubgraph by srcIds、edges、edgeDirection required.
 *
 * <p>query sentence 'GET SUBGRAPH 1 STEPS FROM "player100"'.</p>
 *
 * @author Qi Kai Meng
 */
public class GetSubgraph extends NGqlQuery<GetSubgraph> {
    private long steps = 1;
    private List<Object> srcIds;
    private EdgeDirection edgeDirection = EdgeDirection.BOTH;
    private StringBuffer clause = new StringBuffer();
    private List<String> edges;

    protected GetSubgraph(Graph graph) {
        super(graph);
    }

    public StringBuffer getClause() {
        return clause;
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

    public String connectParameters() {
        if (srcIds == null || srcIds.isEmpty()) {
            throw new InitException("srcIds can not be null");
        }
        StringBuilder result = new StringBuilder();
        result.append(KeyWord.GET_SUBGRAPH).append(" ");
        if (steps >= 0) {
            result.append(steps).append(" ").append(KeyWord.STEPS).append(" ");
        }
        result.append(String.format(KeyWord.FROM + " %s ", Encoding.encodeIdList(srcIds)));
        if (edges != null && !edges.isEmpty()) {
            result.append(edgeDirection.toString()).append(" ").append(String.join(",", edges));
        }
        if (clause != null) {
            result.append(clause);
        }
        System.out.println(result);
        return result.toString().trim();
    }
}
