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
import com.vesoft.nebula.orm.query.cypher.Encoding;
import com.vesoft.nebula.orm.query.util.KeyWord;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * get the attribute value according to the tag and IDs.
 *
 * <p>sentence of query multipoint id multi label is similar to
 * 'FETCH PROP ON player, t1 "player100", "player103"'.</p>
 * <p>sentence of query one point ID one label is similar to
 * 'FETCH PROP ON player "player100"'.</p>
 *
 * @author Qi Kai Meng
 */
public class FetchVertex extends NGqlQuery<FetchVertex> {
    private List<String> tagNames;
    private List<Object> vidList;
    private final Graph graph;

    protected FetchVertex(Graph graph) {
        this.graph = graph;
    }

    public FetchVertex on(String... tagNames) {
        this.tagNames = Arrays.asList(tagNames);
        return this;
    }

    protected FetchVertex init(List<?> vidList) {
        this.vidList = (List<Object>) vidList;
        return this;
    }

    protected FetchVertex init(Object id) {
        this.vidList = new ArrayList<>();
        this.vidList.add(id);
        return this;
    }

    private String connectParameters() {
        if (vidList == null || vidList.isEmpty()) {
            throw new InitException("vidList can not be null");
        }
        StringBuilder result = new StringBuilder();
        result.append(KeyWord.FETCH_PROP_ON).append(" ");
        if (tagNames == null || tagNames.isEmpty()) {
            result.append(KeyWord.ALL).append(" ");
        } else {
            result.append(String.join(",", tagNames)).append(" ");
        }
        result.append(String.join(",", Encoding.encodeIdList(vidList)));
        if (yields != null && !yields.isEmpty()) {
            result.append(" ").append(KeyWord.YIELD).append(" ").append(String.join(",", yields));
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
     * is there data that meets the conditions.
     *
     * @return true or false
     */
    public boolean exist() {
        return !all().isEmpty();
    }
}
