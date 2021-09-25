/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.timetype;

import com.vesoft.nebula.client.graph.data.DateTimeWrapper;
import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.orm.entity.GraphService;
import com.vesoft.nebula.orm.exception.ExecuteException;
import java.util.Objects;

/**
 * this class is a DateTime class.
 *
 * <p>users can pass in time strings like this "2021-08-06T12:23:45:123",
 * it should be noted that users must call {@link #setGraphService(GraphService)}
 * to pass in {@link GraphService} Object,in order to convert the time string into DateTimeWrapper,
 * users can obtain hour / minute / second of local and UTC .</p>
 *
 * @author Qi Kai Meng
 */
public class DateTime {
    private String dateTimeString;
    private DateTimeWrapper dateTimeWrapper;
    private GraphService graphService;

    public void setGraphService(GraphService graphService) {
        this.graphService = graphService;
    }

    public DateTime(String dateTimeString) {
        this.dateTimeString = dateTimeString;
    }

    public void setDateTimeWrapper(DateTimeWrapper dateTimeWrapper) {
        this.dateTimeWrapper = dateTimeWrapper;
    }

    public short getLocalYear() {
        assignDateTimeWrapper();
        return dateTimeWrapper.getLocalDateTime().getYear();
    }

    public byte getLocalMonth() {
        assignDateTimeWrapper();
        return dateTimeWrapper.getLocalDateTime().getMonth();
    }

    public byte getLocalDay() {
        assignDateTimeWrapper();
        return dateTimeWrapper.getLocalDateTime().getDay();
    }

    public byte getLocalHour() {
        assignDateTimeWrapper();
        return dateTimeWrapper.getLocalDateTime().getHour();
    }

    public byte getLocalMinute() {
        assignDateTimeWrapper();
        return dateTimeWrapper.getLocalDateTime().getMinute();
    }

    public byte getLocalSecond() {
        assignDateTimeWrapper();
        return dateTimeWrapper.getLocalDateTime().getSec();
    }

    public int getLocalMillisecond() {
        assignDateTimeWrapper();
        return dateTimeWrapper.getLocalDateTime().getMicrosec();
    }

    public short getUtcYear() {
        assignDateTimeWrapper();
        return dateTimeWrapper.getYear();
    }

    public byte getUtcMonth() {
        assignDateTimeWrapper();
        return dateTimeWrapper.getMonth();
    }

    public byte getUtcDay() {
        assignDateTimeWrapper();
        return dateTimeWrapper.getDay();
    }

    public byte getUtcHour() {
        assignDateTimeWrapper();
        return dateTimeWrapper.getHour();
    }

    public byte getUtcMinute() {
        assignDateTimeWrapper();
        return dateTimeWrapper.getMinute();
    }

    public byte getUtcSecond() {
        assignDateTimeWrapper();
        return dateTimeWrapper.getSecond();
    }

    public int getUtcMillisecond() {
        assignDateTimeWrapper();
        return dateTimeWrapper.getMicrosec();
    }

    private void assignDateTimeWrapper() {
        if (dateTimeWrapper == null) {
            ResultSet result = graphService.run(String.format("YIELD  datetime(\"%s\")", dateTimeString));
            if (!result.isSucceeded()) {
                throw new ExecuteException(result.getErrorMessage());
            }
            dateTimeWrapper = result.rowValues(0).get(0).asDateTime();
        }
    }

    @Override
    public String toString() {
        return dateTimeString;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        DateTime dateTime = (DateTime) o;
        return Objects.equals(dateTimeString, dateTime.dateTimeString);
    }

    @Override
    public int hashCode() {
        return Objects.hash(dateTimeString);
    }
}
