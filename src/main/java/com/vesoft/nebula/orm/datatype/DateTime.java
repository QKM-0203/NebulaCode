/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.datatype;

import com.vesoft.nebula.client.graph.data.DateTimeWrapper;
import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.client.graph.exception.IOErrorException;
import com.vesoft.nebula.client.graph.net.Session;
import com.vesoft.nebula.orm.exception.ExecuteException;

/**
 * this class is a DateTime class.
 *
 * <p>users can pass in time strings like this "2021-08-06T12:23:45:123",
 * it should be noted that users must call setSession to pass in Session Object,
 * in order to convert the time string into DateTimeWrapper, users can obtain
 * hour / minute / second of local and UTC .</p>
 */
public class DateTime {
    private String dateTimeString;
    private DateTimeWrapper dateTimeWrapper;
    private Session session;

    public void setSession(Session session) {
        this.session = session;
    }

    public DateTime(String dateTimeString) {
        this.dateTimeString = dateTimeString;
    }

    public void setDateTimeWrapper(DateTimeWrapper dateTimeWrapper) {
        this.dateTimeWrapper = dateTimeWrapper;
    }

    public short getLocalYear() throws IOErrorException {
        assignDateTimeWrapper();
        return dateTimeWrapper.getLocalDateTime().getYear();
    }

    public byte getLocalMonth() throws IOErrorException {
        assignDateTimeWrapper();
        return dateTimeWrapper.getLocalDateTime().getMonth();
    }

    public byte getLocalDay() throws IOErrorException {
        assignDateTimeWrapper();
        return dateTimeWrapper.getLocalDateTime().getDay();
    }

    public byte getLocalHour() throws IOErrorException {
        assignDateTimeWrapper();
        return dateTimeWrapper.getLocalDateTime().getHour();
    }

    public byte getLocalMinute() throws IOErrorException {
        assignDateTimeWrapper();
        return dateTimeWrapper.getLocalDateTime().getMinute();
    }

    public byte getLocalSecond() throws IOErrorException {
        assignDateTimeWrapper();
        return dateTimeWrapper.getLocalDateTime().getSec();
    }

    public int getLocalMillisecond() throws IOErrorException {
        assignDateTimeWrapper();
        return dateTimeWrapper.getLocalDateTime().getMicrosec();
    }

    public short getUtcYear() throws IOErrorException {
        assignDateTimeWrapper();
        return dateTimeWrapper.getYear();
    }

    public byte getUtcMonth() throws IOErrorException {
        assignDateTimeWrapper();
        return dateTimeWrapper.getMonth();
    }

    public byte getUtcDay() throws IOErrorException {
        assignDateTimeWrapper();
        return dateTimeWrapper.getDay();
    }

    public byte getUtcHour() throws IOErrorException {
        assignDateTimeWrapper();
        return dateTimeWrapper.getHour();
    }

    public byte getUtcMinute() throws IOErrorException {
        assignDateTimeWrapper();
        return dateTimeWrapper.getMinute();
    }

    public byte getUtcSecond() throws IOErrorException {
        assignDateTimeWrapper();
        return dateTimeWrapper.getSecond();
    }

    public int getUtcMillisecond() throws IOErrorException {
        assignDateTimeWrapper();
        return dateTimeWrapper.getMicrosec();
    }

    private void assignDateTimeWrapper() throws IOErrorException {
        if (dateTimeWrapper == null) {
            ResultSet result = session.execute(
                String.format("YIELD  datetime(\"%s\")", dateTimeString));
            if (!result.isSucceeded()) {
                throw new ExecuteException(result.getErrorMessage());
            }
            dateTimeWrapper = result.rowValues(0).get(0).asDateTime();
        }
    }

    public String getDateTimeString() {
        return dateTimeString;
    }

    @Override
    public String toString() {
        return dateTimeString;
    }
}
