/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.datatype;

import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.client.graph.data.TimeWrapper;
import com.vesoft.nebula.orm.entity.GraphService;
import com.vesoft.nebula.orm.exception.ExecuteException;

/**
 * this class is a Time class.
 *
 * <p>users can pass in time strings like this "12:23:45:123",
 * it should be noted that users must call setSession to pass in Session Object,
 * in order to convert the time string into TimeWrapper, users can obtain
 * hour / minute / second of local and UTC.</p>
 */
public class Time {
    private String timeString;
    private TimeWrapper timeWrapper;
    private GraphService graphService;

    public void setGraphService(GraphService graphService) {
        this.graphService = graphService;
    }

    public Time(String timeString) {
        this.timeString = timeString;
    }

    public void setTimeWrapper(TimeWrapper timeWrapper) {
        this.timeWrapper = timeWrapper;
    }

    public int getLocalHour() {
        assignTimeWrapper();
        return timeWrapper.getLocalTime().getHour();
    }

    public int getLocalMinute() {
        assignTimeWrapper();
        return timeWrapper.getLocalTime().getMinute();
    }

    public int getLocalSecond() {
        assignTimeWrapper();
        return timeWrapper.getLocalTime().getSec();
    }

    public int getLocalMillisecond() {
        assignTimeWrapper();
        return timeWrapper.getLocalTime().getMicrosec();
    }

    public int getUtcHour() {
        assignTimeWrapper();
        return timeWrapper.getMicrosec();
    }

    public int getUtcMinute() {
        assignTimeWrapper();
        return timeWrapper.getMinute();
    }

    public int getUtcSecond() {
        assignTimeWrapper();
        return timeWrapper.getSecond();
    }

    public int getUtcMillisecond() {
        assignTimeWrapper();
        return timeWrapper.getMicrosec();
    }

    private void assignTimeWrapper() {
        if (timeWrapper == null) {
            ResultSet result = graphService.run(String.format("YIELD  time(\"%s\")", timeString));
            if (!result.isSucceeded()) {
                throw new ExecuteException(result.getErrorMessage());
            }
            timeWrapper = result.rowValues(0).get(0).asTime();
        }
    }

    public String getTimeString() {
        return timeString;
    }

    @Override
    public String toString() {
        return timeString;
    }
}
