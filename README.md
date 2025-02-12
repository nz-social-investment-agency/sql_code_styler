# SQL code styler Tool
Simple R tool to style SQL files into a consistent format

## Overview
SQL code for analytic projects has value beyond its original use. For example: as an accelerator of future analyses or as a resource for colleagues. However, poor presentation of code is often a barrier to realising this value. To simplify and standardise the process by which SIA ensures SQL code is well presented, we have developed this SQL code styling tool.

The tool makes presentation changes to code by changing line breaks, spacing, location of commas, indentation, and capitalisation. By doing these in a consistent manner, the structure of the code becomes clearer and easy to read. While there are existing tools that serve this purpose, none of these are available within the Stats NZ data lab. Furthermore, online tools are not preferred as (1) they are difficult to batch process, and (2) they require uploading code to the internet.

## Versions

The styling tool is available in two formats:
1. A single file version for ease of use. We recommend that most users download and use the single file version. Instructions for execution are provided at the top of this file. No R knowledge is necessary.
2. A multi-file for ease of development. This version splits the tool across several files for ease of reading and updating. It also includes unit tests for confirming correct performance.

## How to use
The tool is written in R, so you will need R and RStudio installed to use the tool. However, it requires no R knowledge to use.

For the simplest use, download the contents of the **Single file** folder and the documentation from this GitHub repository. Once you have copied these to a suitable location, you can run the tool by following these steps:
1. Open this file in R and "Source" it. Keyboard short cut Ctrl + Shift + S
2. Select file or folder mode
3. Select the file / folder you want styled
4. Select where you want the output saved
5. Wait for the tool to run
6. Review the styled files once the tool completes 

## Citation
Social Investment Agency (2025). SQL code styler. Source code. https://github.com/nz-social-investment-agency/sql_code_styler

## Getting Help
Enquiries can be sent to info@sia.govt.nz
