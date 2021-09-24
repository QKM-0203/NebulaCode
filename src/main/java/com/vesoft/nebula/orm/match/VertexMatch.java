/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.match;

import com.vesoft.nebula.orm.entity.Graph;
import com.vesoft.nebula.orm.query.util.KeyWord;
import java.util.List;
import java.util.Map;

/**
 * <p>you can use {@link #where(Map, String...)} for conditional filtering,
 * and use {@link #skip(long)}, {@link #limit(long)}, {@link #groupBy(List, List)}}
 * and  to operate the output results.</p>
 * <p>the user does not need to consider the calling order,
 * when the user calls the {@link #all()}、{@link #first()} etc method,
 * the parameter connection will be made.</p>
 * <p>note: make sure that at least one index is available for the match statement.</p>
 *
 * @author Qi Kai Meng
 */
public class VertexMatch extends Match<VertexMatch> {
    private String tagName;
    private Map<String, Object> propMap;

    protected VertexMatch(Graph graph) {
        super(graph);
    }


    /**
     * if you don't set the index, you can find it by id (v) at after where.
     *
     * @param tagName if vertex has tag index,you can pass in,eg:
     *                match (v:player),can be null eg: match (v).
     * @param propMap if you create tag prop index,you can pass in propMap
     *                eg: match (v:player{name: "qkm"}),can be null eg:
     *                match (v:player).
     */
    protected VertexMatch init(String tagName, Map<String, Object> propMap) {
        this.tagName = tagName;
        this.propMap = propMap;
        return this;
    }

    /**
     * connect parameters.
     *
     * @return sentence
     */
    public String connectParameters() {
        StringBuilder result = new StringBuilder();
        result.append(String.format(KeyWord.MATCH + " (v%s)", joinTag(tagName, propMap)));
        result.append(judgeAndJoinWhere(conMap, filterString, 0));
        result.append(joinGroupByAndOrderBy(groupBy, aggregateFunctions, orderByMatch, 0));
        result.append(joinSkipAndLimit(skip, limit));
        System.out.println(result);
        return result.toString().trim();
    }
}
