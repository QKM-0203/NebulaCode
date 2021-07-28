/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package entity;

import operator.DateType;

/**
 * create a space.
 *
 */
public class Space {
  private String spaceName;
  private int partitionNumber = 100;
  private int replicaFactor = 1;
  private DateType vidDateType = DateType.FIXED_STRING;

  /**
   * create space by constructor.
   *
   * @param spaceName  spaceName
   * @param partitionNumber  the number of partitions in graph space
   * @param replicaFactor number of copies per slice
   * @param vidDateType optional value for data type of id is FIXED_STRING(N)和INT64
   */
  public Space(String spaceName, int partitionNumber, int replicaFactor, DateType vidDateType) {
    this.spaceName = spaceName;
    this.partitionNumber = partitionNumber;
    this.replicaFactor = replicaFactor;
    this.vidDateType = vidDateType;
  }

  public String getSpaceName() {
    return spaceName;
  }

  public void setSpaceName(String spaceName) {
    this.spaceName = spaceName;
  }

  public void setPartitionNumber(int partitionNumber) {
    this.partitionNumber = partitionNumber;
  }

  public void setReplicaFactor(int replicaFactor) {
    this.replicaFactor = replicaFactor;
  }

  public void setVidType(DateType vidType) {
    this.vidDateType = vidType;
  }

  public int getPartitionNumber() {
    return partitionNumber;
  }

  public int getReplicaFactor() {
    return replicaFactor;
  }

  public DateType getVidDateType() {
    return vidDateType;
  }
}