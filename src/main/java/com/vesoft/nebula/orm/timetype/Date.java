/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.timetype;

import com.vesoft.nebula.client.graph.data.DateWrapper;
import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.orm.entity.GraphService;
import com.vesoft.nebula.orm.exception.ExecuteException;
import java.util.Objects;

/**
 * this class is a Date class.
 *
 * <p>users can pass in time strings like this "2021-08-06",
 * it should be noted that users must call {@link #setGraphService(GraphService)}
 * to pass in {@link GraphService} Object,in order to convert the time string into DateWrapper,
 * users can obtain year / month / day of dateString.</p>
 *
 * @author Qi Kai Meng
 */
public class Date {
    private String dateString;
    private DateWrapper dateWrapper;
    private GraphService graphService;

    public void setGraphService(GraphService graphService) {
        this.graphService = graphService;
    }

    public Date(String dateString) {
        this.dateString = dateString;
    }

    public void setDateWrapper(DateWrapper dateWrapper) {
        this.dateWrapper = dateWrapper;
    }

    public short getYear() {
        assignDateWrapper();
        return dateWrapper.getYear();
    }

    public byte getMonth() {
        assignDateWrapper();
        return dateWrapper.getMonth();
    }

    public byte getDay() {
        assignDateWrapper();
        return dateWrapper.getDay();
    }

    private void assignDateWrapper() {
        if (dateWrapper == null) {
            ResultSet result = graphService.run(String.format("YIELD  date(\"%s\")", dateString));
            if (!result.isSucceeded()) {
                throw new ExecuteException(result.getErrorMessage());
            }
            dateWrapper = result.rowValues(0).get(0).asDate();
        }
    }

    @Override
    public String toString() {
        return dateString;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        Date date = (Date) o;
        return Objects.equals(dateString, date.dateString);
    }

    @Override
    public int hashCode() {
        return Objects.hash(dateString);
    }
}
