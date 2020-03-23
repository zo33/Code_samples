# set up environment
from selenium import webdriver
import os
import time
import csv
import math
import pandas as pd
import re
import numpy as np
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import Select
import random
from selenium.common.exceptions import NoSuchElementException
import operator

## This was my first project using Python for webscraping (previously I have only used
## Python for implementing keras neural networks).



## STEP 1: Create dataframe of all women in Notre Dame's class of 2016 using the ND Alumni website.
# The website groups alumni by dorm (single sex), so scrape basic name / etc. info for women's dorms occupants.

    # specifies the path to the chromedriver.exe
    path = os.getcwd()
    driver = webdriver.Chrome(path + '/chromedriver')

    # driver.get method() will navigate to a page given by the URL address
    driver.get('https://securelb.imodules.com/s/1210/myND/landing-2col-wide.aspx?sid=1210&gid=1&pgid=3&returnurl=http%3a%2f%2fmy.nd.edu%2fs%2f1210%2fmyND%2fmynd-start.aspx')

    # locate email form by_class_name
    username = driver.find_element_by_class_name('inputLoginUsername')

    # send_keys() to simulate key strokes
    username.send_keys('YOUR LOGIN INFO HERE')

    # locate password form by_class_name
    password = driver.find_element_by_class_name('inputLoginPassword')

    # send_keys() to simulate key strokes
    password.send_keys('YOUR PASSWORD HERE')

    # .click() to mimic button click
    driver.find_element_by_xpath('//*[@id="cid_40_btnLogin"]').click()

    # go to each of 14 women's dorm pages for c/0 2016, extract name/ year for each entry from all the pages
    # can't do it in one loop because of search return limits

    womens_dorms = ['Badin Hall','Breen-Phillips Hall', 'Cavanaugh Hall', 'Farley Hall', 'Howard Hall','Lewis Hall',
                    'Lyons Hall','McGlinn Hall','ND Off-Campus Housing','Pangborn Hall','Pasquerilla East Hall','Pasquerilla West Hall',
                    'Ryan Hall','Walsh Hall','Welsh Family Hall']

    names = []
    years = []
    dorms = []


    for dorm in womens_dorms:

        driver.get('http://my.nd.edu/s/1210/myND/interior-2col.aspx?sid=1210&gid=1&pgid=6#/Search/Simple')
        time.sleep(2)

        residence_hall = Select(driver.find_element_by_id('mf_551'))
        residence_hall.deselect_all()
        residence_hall.select_by_visible_text(dorm)
        #residence_hall.select_by_visible_text('Breen-Phillips Hall')

        year = Select(driver.find_element_by_xpath('//*[@id="mf_6690"]'))
        year.select_by_visible_text('2016')

        driver.find_element_by_xpath('//*[@id="imod-view-content"]/div[3]/div[7]/button').click()
        time.sleep(5)

        number_results = driver.find_element_by_xpath('//*[@id="imod-view-content"]/div[2]/p/strong')
        results_pages = math.ceil((int(number_results.text))/20)

        for page in range(1,(results_pages+1)):
            url = 'http://my.nd.edu/s/1210/myND/interior-2col.aspx?sid=1210&gid=1&pgid=6#/Search/Results/Simple/p/{}#gridStart'.format(page)
            driver.get(url)
            time.sleep(5)
            result_entries = driver.find_elements_by_class_name('imod-directory-result-wrapper.layout4.ng-scope')

            for result in result_entries:

                name_to_add = result.find_elements_by_class_name('imod-directory-member-name')
                names.append(name_to_add[0].text)

                year_to_add = result.find_elements_by_class_name('imod-directory-member-classyear')
                years.append(year_to_add[0].text)

            for results in result_entries:

                dorms.append(dorm)

    for i in range(0,len(names)):
        names[i] = re.sub('".*?"', '', names[i])

    for i in range(0, len(names)):
        names[i] = re.sub('[\(\[]is lost[\)\]]', '', names[i])

    for i in range(0, len(names)):
        names[i] = re.sub(' +', ' ', names[i])

    women_df = pd.DataFrame(
        {'Name': names,
        'Grad_Year': years,
        'Dorm': dorms
        })

    women_df['Grad_Year'].value_counts()
    women_df_2016 = women_df[women_df.Grad_Year == '2016']
    women_df_2016.to_csv('ND_2016_Women_all.csv')

    parentheses_name = [x for x in women_df_2016['Name'] if '(' in x]

    women_df_2016_auto = women_df_2016[np.logical_not(women_df_2016['Name'].isin(parentheses_name))]
    women_df_2016_auto.to_csv('ND_2016_Women_auto.csv')

    women_df_2016_manual = women_df_2016[women_df_2016['Name'].isin(parentheses_name)]
    women_df_2016_manual.to_csv('ND_2016_Women_manual.csv')

## STEP 2: Find linkedin profiles for this list of names

# Navigates to Linkedin and logs you in: Required for all further steps that involve extracting data from linkedin

    # specifies the path to the chromedriver.exe
    path = os.getcwd()
    driver = webdriver.Chrome(path + '/chromedriver')

    # driver.get method() will navigate to a page given by the URL address
    driver.get('https://www.linkedin.com')

    # locate email form by_class_name
    username = driver.find_element_by_name('session_key')

    # send_keys() to simulate key strokes
    username.send_keys('ENTER YOUR OWN LOGIN INFO HERE')

    # locate password form by_class_name
    password = driver.find_element_by_name('session_password')

    # send_keys() to simulate key strokes
    password.send_keys('ENTER YOUR OWN PASSWORD HERE')

    # .click() to mimic button click
    driver.find_element_by_xpath('/html/body/nav/section[2]/form/div[2]/button').click()


# Go to linkedin University of ND page and search alumni for name
# Later checked ND graduation date against 2016 -> Once profile information was scraped

    auto = pd.read_csv('ND_2016_Women_auto.csv')
    auto_scrape_names = list(auto['Name'])

    women_linkedin_urls = []
    matching_names_to_urls = []

    #for person in auto_scrape_names:
    for person in auto_scrape_names:

        driver.get('https://www.linkedin.com/school/university-of-notre-dame/people/')
        time.sleep(random.randint(3,6))
        search_box = driver.find_element_by_id('people-search-keywords')
        search_box.send_keys(person)
        time.sleep(random.randint(3,6))
        search_box.send_keys(Keys.ENTER)
        time.sleep(random.randint(3,6))

        search_results = driver.find_elements_by_class_name('org-people-profile-card.ember-view')

        if len(search_results) == 0:

            women_linkedin_urls.append('Profile not found')
            time.sleep(2)

        elif len(search_results) == 1:

            name_block = search_results[0].find_element_by_class_name('org-people-profile-card__profile-title.t-black.lt-line-clamp.lt-line-clamp--single-line.ember-view')
            card_name = name_block.text

            if (card_name in person or person in card_name) == True:

                url_block = search_results[0].find_elements_by_class_name('link-without-visited-state.ember-view')
                for b in url_block:
                    women_linkedin_urls.append(b.get_attribute("href"))
                    time.sleep(2)

            else:

                women_linkedin_urls.append('Name does not match results')
                time.sleep(2)

        else:

            url_to_add = None

            for result in search_results: # Match name provided for search with name returned

                name_block = result.find_element_by_class_name('org-people-profile-card__profile-title.t-black.lt-line-clamp.lt-line-clamp--single-line.ember-view')
                card_name = name_block.text
                time.sleep(2)

                if (card_name in person or person in card_name) == True:

                    url_block = result.find_elements_by_class_name('link-without-visited-state.ember-view')
                    for b in url_block:
                        url_to_add = b.get_attribute("href")
                    break

                else:

                    continue

            if url_to_add is None:

                women_linkedin_urls.append('Name does not match results')

            else:

                women_linkedin_urls.append(url_to_add)


         matching_names_to_urls.append(person)
         time.sleep(random.randint(5,10))

    rows = zip(matching_names_to_urls,women_linkedin_urls)
    with open('test.csv', 'w') as f:
        writer = csv.writer(f)
        for row in rows:
            writer.writerow(row)




## STEP 3 : Scrape information from the Class of 2016 profiles in URL list
# The scrape code itself is very convoluted due to the vast number of profile options
# regarding what sections different people have / if you have to click through to show more
# of a section / if there are dates associated with different linkedin entries

    def hasNumbers(inputString):
        return any(char.isdigit() for char in inputString)

    def cleanDates(inputString, column):
        if column == 'Degree_dates':
            return inputString.replace('Dates attended or expected graduation\n', '')
        elif column == 'Job_dates':
            return inputString.replace('Dates Employed\n', '')
        elif column == 'Volunteer_dates':
            return inputString.replace('Dates volunteered\n', '')

    def cleanText(inputString, type):
        if type == 'Company':
            return inputString.replace('Company Name\n', '')
        elif type == 'Position':
            return inputString.replace('Title\n', '')
        elif type == 'Degree':
            return inputString.replace('Degree Name\n', '')
        elif type == 'Volunteer':
            return inputString.replace('Company Name\n', '')
        else:
            return inputString


    # first make sure unique list of URLS

    full = pd.read_csv('ND_2016_Women_URLS.csv')
    full_urls = list(full['URL'])
    full_urls_found = list(full_urls[0:778])
    full_urls_found_unique =  list(set(list(full_urls_found)))

    # Information to store:

    info_to_store = ['URL',
                    'Title',

                    'Company',
                     'Position',
                     'Job_started',
                     'Job_ended',

                     'School',
                     'Degree',
                     'Degree_started',
                     'Degree_ended',

                     'Volunteer_Organization',
                     'Volunteer_Position',
                     'Volunteer_started',
                     'Volunteer_ended',

                     'Awards',
                     'Publications',
                     'Certifications']

    # Nested dictionary to store this information : run x at a time in case of errors

    test = False

    if test is True:

        test_urls = ['https://www.linkedin.com/in/zoe-volenec-1a1ba6a7/', 'https://www.linkedin.com/in/meghan-m-pfeifer/',
             'https://www.linkedin.com/in/claire-purcell-730972139/',
             'https://www.linkedin.com/in/stephanie-terpening-4825a06b/',
             'https://www.linkedin.com/in/rmwallace10/',
             'https://www.linkedin.com/in/pattisong/',
             'https://www.linkedin.com/in/elizabethmiggins/']

        dic_dic = {}
        for x in range(0, len(test_urls)):
            level = x
            dic_dic[level] = {}
            for entry in info_to_store:
                dic_dic[level][entry] = []
    else:

        dic_dic = {}
        for x in range(0, len(full_urls_found_unique)):
            level = x
            dic_dic[level] = {}
            for entry in info_to_store:
                dic_dic[level][entry] = []


    for i in range(0, len(full_urls_found_unique)):

        # navigate to page
        url = full_urls_found_unique[i]
        #url = test_urls[i]
        driver.get(url)
        time.sleep(random.randint(5,10))

        dic_dic[i]['URL'] = url

        # scroll all the way down to load full page
        elem = driver.find_element_by_tag_name("body")
        no_of_pagedowns = 10
        while no_of_pagedowns:
            elem.send_keys(Keys.PAGE_DOWN)
            time.sleep(random.randint(5,10))
            no_of_pagedowns -= 1

        time.sleep(random.randint(5, 10))

        # Title
        title_block = driver.find_element_by_class_name('mt1.t-18.t-black.t-normal')

        if len(title_block.text) is not 0:

            dic_dic[i]['Title'] = title_block.text

        else:

            dic_dic[i]['Title'] = 'No title provided'


        # Job experience

        try:

            experience_block = driver.find_element_by_id('experience-section')

        except NoSuchElementException:

            dic_dic[i]['Company'] = 'No experience data provided'

            dic_dic[i]['Position'] = 'No experience data provided'

            dic_dic[i]['Job_started'] = 'No experience data provided'

            dic_dic[i]['Job_ended'] = 'No experience data provided'

        else:

            if len(experience_block.find_elements_by_class_name('pv-profile-section__see-more-inline.pv-profile-section__text-truncate-toggle.link')) > 0:
                button = experience_block.find_element_by_tag_name('button')
                button.send_keys(webdriver.common.keys.Keys.SPACE)

                elem = driver.find_element_by_tag_name("body")
                no_of_pagedowns = 5
                while no_of_pagedowns:
                    elem.send_keys(Keys.PAGE_DOWN)
                    time.sleep(random.randint(1, 5))
                    no_of_pagedowns -= 1

            experience_blocks_expanded = experience_block.find_elements_by_class_name('pv-profile-section__sortable-item.pv-profile-section__section-info-item.relative.pv-profile-section__list-item.sortable-item.ember-view')
            experience_blocks_expanded.extend(experience_block.find_elements_by_class_name('pv-entity__position-group-pager.pv-profile-section__list-item.ember-view'))

            for experience in experience_blocks_expanded:

                try: # are their multiple positions for one experience ?

                    experience.find_element_by_class_name('pv-entity__position-group.mt2')

                except NoSuchElementException:

                    try: # apparently some people don't have dates on experiences ...

                        dates = experience.find_element_by_class_name('pv-entity__date-range.t-14.t-black--light.t-normal')

                    except NoSuchElementException:

                        company = experience.find_element_by_class_name('pv-entity__secondary-title')
                        dic_dic[i]['Company'].append(cleanText(company.text, 'Company'))

                        position = experience.find_element_by_tag_name('h3')
                        dic_dic[i]['Position'].append(cleanText(position.text, 'Position'))

                        dic_dic[i]['Job_started'] = 'No experience dates provided for the job'

                        dic_dic[i]['Job_ended'] = 'No experience dates provided for the job'

                    else:
                        dates_text_cleaned = cleanDates(dates.text, 'Job_dates')

                        # Extract date information to narrow down to post-grad experiences
                        date_started_string = dates_text_cleaned.split(' – ')[0].split(' ')

                        if len(date_started_string) == 2: # are month and year info provided ?

                            date_started = int(date_started_string[1])

                        else:

                            date_started = int(date_started_string[0])

                        try:

                            date_ended_string = dates_text_cleaned.split(' – ')[1].split(' ')

                        except IndexError:

                            if len(date_started_string) == 2:  # are month and year info provided ?

                                date_ended = int(date_started_string[1])

                            else:

                                date_ended = int(date_started_string[0])
                        else:


                            if len(date_ended_string) == 2:

                                date_ended = int(date_ended_string[1])


                            else:

                                if date_ended_string[0] == 'Present':

                                    date_ended =  2050

                                else:

                                    date_ended = int(date_ended_string[0])

                        if date_started > 2015 or date_ended > 2016:

                            dic_dic[i]['Job_started'].append(date_started)
                            dic_dic[i]['Job_ended'].append(date_ended)

                            company = experience.find_element_by_class_name('pv-entity__secondary-title')
                            dic_dic[i]['Company'].append(cleanText(company.text, 'Company'))

                            position = experience.find_element_by_tag_name('h3')
                            dic_dic[i]['Position'].append(cleanText(position.text, 'Position'))


                else:

                    multiple_positions = experience.find_elements_by_class_name('pv-entity__position-group-role-item.sortable-item.ember-view')
                    multiple_positions.extend(experience.find_elements_by_class_name('pv-entity__position-group-role-item'))

                    for position_listing in multiple_positions:

                        dates = position_listing.find_element_by_css_selector('h4.pv-entity__date-range.t-14.t-black--light.t-normal')

                        dates_text_cleaned = cleanDates(dates.text, 'Job_dates')

                        # Extract date information to narrow down to post-grad experiences
                        date_started_string = dates_text_cleaned.split(' – ')[0].split(' ')

                        if len(date_started_string) == 2:  # are month and year info provided ?

                            date_started = int(date_started_string[1])

                        else:

                            date_started = int(date_started_string[0])

                        try:

                            date_ended_string = dates_text_cleaned.split(' – ')[1].split(' ')

                        except IndexError:

                            if len(date_started_string) == 2:  # are month and year info provided ?

                                date_ended = int(date_started_string[1])

                            else:

                                date_ended = int(date_started_string[0])
                        else:

                            if len(date_ended_string) == 2:

                                date_ended = int(date_ended_string[1])


                            else:

                                if date_ended_string[0] == 'Present':

                                    date_ended = 2050

                                else:

                                    date_ended = int(date_ended_string[0])

                        if date_started > 2015 or date_ended > 2016:

                            dic_dic[i]['Job_started'].append(date_started)
                            dic_dic[i]['Job_ended'].append(date_ended)

                            company = experience.find_element_by_css_selector('h3.t-16.t-black.t-bold')
                            dic_dic[i]['Company'].append(cleanText(company.text, 'Company'))

                            position = position_listing.find_element_by_css_selector('h3.t-14.t-black.t-bold')
                            dic_dic[i]['Position'].append(cleanText(position.text, 'Position'))


        time.sleep(random.randint(5, 10))

        # Education

        try:

            education_block = driver.find_element_by_id('education-section')

        except NoSuchElementException:

            dic_dic[i]['School'] = 'No school data provided'

            dic_dic[i]['Degree'] = 'No school data provided'

            dic_dic[i]['Degree_start'] = 'No school data provided'

            dic_dic[i]['Degree_end'] = 'No school data provided'

        else:

            if len(education_block.find_elements_by_class_name('pv-profile-section__see-more-inline.pv-profile-section__text-truncate-toggle.link')) > 0:
                button = education_block.find_element_by_tag_name('button')
                button.send_keys(webdriver.common.keys.Keys.SPACE)

                elem = driver.find_element_by_tag_name("body")
                no_of_pagedowns = 5
                while no_of_pagedowns:
                    elem.send_keys(Keys.PAGE_DOWN)
                    time.sleep(random.randint(1,5))
                    no_of_pagedowns -= 1


            education_block_expanded = education_block.find_elements_by_class_name('pv-profile-section__sortable-item.pv-profile-section__section-info-item.relative.pv-profile-section__sortable-item--v2.pv-profile-section__list-item.sortable-item.ember-view')

            if len(education_block_expanded) == 0:

                try:

                    degree_dates = education_block.find_element_by_class_name('pv-entity__dates.t-14.t-black--light.t-normal')

                except NoSuchElementException:  # even if no date information include schooling

                    # see if date is hidden in degree

                    try:

                        degree = education_block.find_element_by_class_name('pv-entity__secondary-title.pv-entity__degree-name.pv-entity__secondary-title.t-14.t-black.t-normal')

                    except NoSuchElementException:

                        dic_dic[i]['Degree_started'].append('No degree dates information provided')
                        dic_dic[i]['Degree_ended'].append('No degree dates information provided')

                        dic_dic[i]['Degree'].append('No degree information provided')

                        school = education_block.find_element_by_class_name('pv-entity__school-name.t-16.t-black.t-bold')
                        dic_dic[i]['School'].append(school.text)

                    else:

                        if hasNumbers(degree.text) is True:

                            dates_text_cleaned = cleanText(degree.text, 'Degree')

                            # Extract date information to narrow down to post-grad experiences
                            date_started_string = dates_text_cleaned.split(' - ')[0].split(' ')

                            if len(date_started_string) == 2:  # are month and year info provided ?

                                date_started = int(date_started_string[1])

                            else:

                                if hasNumbers(date_started_string[0]):

                                    date_started = int(date_started_string[0])

                                else:

                                    date_started = 'NA'

                            try:

                                date_ended_string = dates_text_cleaned.split(' - ')[1].split(' ')

                            except IndexError:

                                if len(date_started_string) == 2:  # are month and year info provided ?

                                    date_ended = int(date_started_string[1])

                                else:

                                    date_ended = int(date_started_string[0])
                            else:

                                if len(date_ended_string) == 2:

                                    date_ended = int(date_ended_string[1])


                                else:

                                    if date_ended_string[0] == 'Present':

                                        date_ended = 2050

                                    else:

                                        date_ended = int(date_ended_string[0])

                            if date_started > 2015 or date_ended > 2016:

                                dic_dic[i]['Degree_started'].append(date_started)
                                dic_dic[i]['Degree_ended'].append(date_ended)

                                school = education_block.find_element_by_class_name('pv-entity__school-name.t-16.t-black.t-bold')
                                dic_dic[i]['School'].append(school.text)

                                dic_dic[i]['Degree'].append('No degree information provided')


                else:

                    # go through check to see if degree dates are post grad before including
                    dates_text_cleaned = cleanDates(degree_dates.text, 'Degree_dates')

                    # Extract date information to narrow down to post-grad experiences
                    date_started_string = dates_text_cleaned.split(' – ')[0].split(' ')

                    if len(date_started_string) == 2:  # are month and year info provided ?

                        date_started = int(date_started_string[1])

                    else:

                        if hasNumbers(date_started_string[0]):

                            date_started = int(date_started_string[0])

                        else:

                            date_started = 'NA'

                    try:

                        date_ended_string = dates_text_cleaned.split(' – ')[1].split(' ')

                    except IndexError:

                        if len(date_started_string) == 2:  # are month and year info provided ?

                            date_ended = int(date_started_string[1])

                        else:

                            date_ended = int(date_started_string[0])
                    else:

                        if len(date_ended_string) == 2:

                            date_ended = int(date_ended_string[1])


                        else:

                            if date_ended_string[0] == 'Present':

                                date_ended = 2050

                            else:

                                date_ended = int(date_ended_string[0])

                    if date_started == 'NA':

                        date_started = date_ended

                    if date_started > 2015 or date_ended > 2016:

                        dic_dic[i]['Degree_started'].append(date_started)
                        dic_dic[i]['Degree_ended'].append(date_ended)

                        school = education_block.find_element_by_class_name('pv-entity__school-name.t-16.t-black.t-bold')
                        dic_dic[i]['School'].append(school.text)

                        try:

                            degree = education_block.find_element_by_class_name('pv-entity__secondary-title.pv-entity__degree-name.pv-entity__secondary-title.t-14.t-black.t-normal')

                        except NoSuchElementException:

                            dic_dic[i]['Degree'].append('No degree information provided')

                        else:

                            dic_dic[i]['Degree'].append(cleanText(degree.text,'Degree'))


            else:

                for education in education_block_expanded:

                    try:

                        degree_dates = education.find_element_by_class_name('pv-entity__dates.t-14.t-black--light.t-normal')

                    except NoSuchElementException:

                        # first test to see if it date is hidden in degree text
                        # if so, run through sequence to check years

                        try:

                            degree = education.find_element_by_class_name('pv-entity__secondary-title.pv-entity__degree-name.pv-entity__secondary-title.t-14.t-black.t-normal')

                        except NoSuchElementException: # so there isn't degree or date information -> still include

                            dic_dic[i]['Degree'].append('No degree information provided')

                            dic_dic[i]['Degree_started'].append('No degree dates information provided')
                            dic_dic[i]['Degree_ended'].append('No degree dates information provided')

                            school = education.find_element_by_tag_name('h3')
                            dic_dic[i]['School'].append(school.text)

                        else:

                            if hasNumbers(degree.text) is True:

                                dates_text_cleaned = cleanText(degree.text, 'Degree')

                                # Extract date information to narrow down to post-grad experiences
                                date_started_string = dates_text_cleaned.split(' - ')[0].split(' ')

                                if len(date_started_string) == 2:  # are month and year info provided ?

                                    date_started = int(date_started_string[1])

                                else:

                                    if hasNumbers(date_started_string[0]):

                                        date_started = int(date_started_string[0])

                                    else:

                                        date_started = 'NA'

                                try:

                                    date_ended_string = dates_text_cleaned.split(' - ')[1].split(' ')

                                except IndexError:

                                    if len(date_started_string) == 2:  # are month and year info provided ?

                                        date_ended = int(date_started_string[1])

                                    else:

                                        date_ended = int(date_started_string[0])
                                else:

                                    if len(date_ended_string) == 2:

                                        date_ended = int(date_ended_string[1])


                                    else:

                                        if date_ended_string[0] == 'Present':

                                            date_ended = 2050

                                        else:

                                            date_ended = int(date_ended_string[0])

                                if date_started == 'NA':

                                    date_started = date_ended

                                if date_started > 2015 or date_ended > 2016:

                                    dic_dic[i]['Degree_started'].append(date_started)
                                    dic_dic[i]['Degree_ended'].append(date_ended)

                                    school = education.find_element_by_tag_name('h3')
                                    dic_dic[i]['School'].append(school.text)


                                    dic_dic[i]['Degree'].append('No degree information provided')

                            else:

                                dic_dic[i]['Degree'].append(cleanText(degree.text, 'Degree'))

                                school = education.find_element_by_tag_name('h3')
                                dic_dic[i]['School'].append(school.text)

                                dic_dic[i]['Degree_started'].append('No degree dates information provided')
                                dic_dic[i]['Degree_ended'].append('No degree dates information provided')


                    else:

                        # Run through sequence to check years

                        dates_text_cleaned = cleanDates(degree_dates.text, 'Degree_dates')

                        # Extract date information to narrow down to post-grad experiences
                        date_started_string = dates_text_cleaned.split(' – ')[0].split(' ')

                        if len(date_started_string) == 2:  # are month and year info provided ?

                            date_started = int(date_started_string[1])

                        else:

                            if hasNumbers(date_started_string[0]):

                                date_started = int(date_started_string[0])

                            else:

                                date_started = 'NA'

                        try:

                            date_ended_string = dates_text_cleaned.split(' – ')[1].split(' ')

                        except IndexError:

                            if len(date_started_string) == 2:  # are month and year info provided ?

                                date_ended = int(date_started_string[1])

                            else:

                                date_ended = int(date_started_string[0])
                        else:

                            if len(date_ended_string) == 2:

                                date_ended = int(date_ended_string[1])


                            else:

                                if date_ended_string[0] == 'Present':

                                    date_ended = 2050

                                else:

                                    date_ended = int(date_ended_string[0])

                        if date_started == 'NA':

                            date_started = date_ended

                        if date_started > 2015 or date_ended > 2016:

                            dic_dic[i]['Degree_started'].append(date_started)
                            dic_dic[i]['Degree_ended'].append(date_ended)

                            school = education.find_element_by_tag_name('h3')
                            dic_dic[i]['School'].append(school.text)

                            try:

                                degree = education.find_element_by_class_name('pv-entity__secondary-title.pv-entity__degree-name.pv-entity__secondary-title.t-14.t-black.t-normal')

                            except NoSuchElementException:

                                dic_dic[i]['Degree'].append('No degree information provided')

                            else:

                                dic_dic[i]['Degree'].append(cleanText(degree.text,'Degree'))


        time.sleep(random.randint(5, 10))

        # Volunteer work

        try:

            volunteer_block = driver.find_element_by_class_name('pv-profile-section.volunteering-section.ember-view')

        except NoSuchElementException:

            dic_dic[i]['Volunteer_Organization'] = 'No volunteer data provided'

            dic_dic[i]['Volunteer_Position'] = 'No volunteer data provided'

            dic_dic[i]['Volunteer_started'] = 'No volunteer data provided'

            dic_dic[i]['Volunteer_ended'] = 'No volunteer data provided'

        else:

            volunteer_positions = volunteer_block.find_elements_by_class_name('pv-profile-section__sortable-item.pv-profile-section__section-info-item.relative.pv-profile-section__sortable-item--v2.pv-profile-section__list-item.sortable-item.ember-view')

            if len(volunteer_positions) == 0:

                try:

                    volunteer_dates = volunteer_block.find_element_by_class_name('pv-entity__date-range.detail-facet.inline-block.t-14.t-black--light.t-normal')

                except NoSuchElementException:

                    organization = volunteer_block.find_element_by_class_name('t-14.t-black.t-normal')
                    dic_dic[i]['Volunteer_Organization'] = cleanText(organization.text, 'Volunteer')

                    try:

                        position = volunteer_block.find_element_by_tag_name('h3')

                    except NoSuchElementException:

                        dic_dic[i]['Volunteer_Position'] = 'No volunteer position data provided'

                    else:

                        dic_dic[i]['Volunteer_Position'] = position.text

                else:
                    # Run through sequence to check years

                    dates_text_cleaned = cleanDates(volunteer_dates.text, 'Volunteer_dates')

                    # Extract date information to narrow down to post-grad experiences
                    date_started_string = dates_text_cleaned.split(' – ')[0].split(' ')

                    if len(date_started_string) == 2:  # are month and year info provided ?

                        date_started = int(date_started_string[1])

                    else:

                        date_started = int(date_started_string[0])

                    try:

                        date_ended_string = dates_text_cleaned.split(' – ')[1].split(' ')

                    except IndexError:

                        if len(date_started_string) == 2:  # are month and year info provided ?

                            date_ended = int(date_started_string[1])

                        else:

                            date_ended = int(date_started_string[0])
                    else:

                        if len(date_ended_string) == 2:

                            date_ended = int(date_ended_string[1])


                        else:

                            if date_ended_string[0] == 'Present':

                                date_ended = 2050

                            else:

                                date_ended = int(date_ended_string[0])

                    if date_started > 2015 or date_ended > 2016:

                        dic_dic[i]['Volunteer_started'].append(date_started)
                        dic_dic[i]['Volunteer_ended'].append(date_ended)


                        organization = volunteer_block.find_element_by_class_name('t-14.t-black.t-normal')
                        dic_dic[i]['Volunteer_Organization'] = cleanText(organization.text,'Volunteer')

                        try:

                            position = volunteer_block.find_element_by_tag_name('h3')

                        except NoSuchElementException:

                            dic_dic[i]['Volunteer_Position'] = 'No volunteer position data provided'

                        else:

                            dic_dic[i]['Volunteer_Position'] = position.text


            else:

                for volunteer in volunteer_positions:

                    try:

                        volunteer_dates = volunteer.find_element_by_class_name('pv-entity__date-range.detail-facet.inline-block.t-14.t-black--light.t-normal')

                    except NoSuchElementException:

                        organization = volunteer.find_element_by_class_name('pv-entity__secondary-title')
                        dic_dic[i]['Volunteer_Organization'].append(cleanText(organization.text, 'Volunteer'))

                        try:

                            position = volunteer.find_element_by_tag_name('h3')

                        except NoSuchElementException:

                            dic_dic[i]['Volunteer_Position'].append('No volunteer position data provided')

                        else:

                            dic_dic[i]['Volunteer_Position'].append(position.text)

                    else:

                        dates_text_cleaned = cleanDates(volunteer_dates.text, 'Volunteer_dates')

                        # Extract date information to narrow down to post-grad experiences
                        date_started_string = dates_text_cleaned.split(' – ')[0].split(' ')

                        if len(date_started_string) == 2:  # are month and year info provided ?

                            date_started = int(date_started_string[1])

                        else:

                            date_started = int(date_started_string[0])

                        try:

                            date_ended_string = dates_text_cleaned.split(' – ')[1].split(' ')

                        except IndexError:

                            if len(date_started_string) == 2:  # are month and year info provided ?

                                date_ended = int(date_started_string[1])

                            else:

                                date_ended = int(date_started_string[0])
                        else:

                            if len(date_ended_string) == 2:

                                date_ended = int(date_ended_string[1])


                            else:

                                if date_ended_string[0] == 'Present':

                                    date_ended = 2050

                                else:

                                    date_ended = int(date_ended_string[0])

                        if date_started > 2015 or date_ended > 2016:

                            dic_dic[i]['Volunteer_started'].append(date_started)
                            dic_dic[i]['Volunteer_ended'].append(date_ended)

                            organization = volunteer.find_element_by_class_name('pv-entity__secondary-title')
                            dic_dic[i]['Volunteer_Organization'].append(cleanText(organization.text,'Volunteer'))

                            try:

                                position = volunteer.find_element_by_tag_name('h3')

                            except NoSuchElementException:

                                dic_dic[i]['Volunteer_Position'].append('No volunteer position data provided')

                            else:

                                dic_dic[i]['Volunteer_Position'].append(position.text)


        time.sleep(random.randint(5, 10))

        # Licenses and Certifications

        try:

            lc = driver.find_element_by_id('certifications-section')

        except NoSuchElementException:

            dic_dic[i]['Certifications'] = 'No certifications data provided'

        else:

            lc_names = lc.find_elements_by_tag_name('h3')

            for lc_entry in lc_names:

                dic_dic[i]['Certifications'].append(lc_entry.text)


        # Accomplishments Section

        try:

            accomplishments_block = driver.find_element_by_class_name('pv-profile-section.pv-accomplishments-section.artdeco-container-card.ember-view')

        except NoSuchElementException:

            dic_dic[i]['Awards'] = 'No awards data provided'
            dic_dic[i]['Publications'] = 'No publications data provided'

        else:

            # Awards
            try:

                awards_block = accomplishments_block.find_element_by_id('honors-expandable-content')

            except NoSuchElementException:

                dic_dic[i]['Awards'] = 'No awards data provided'

            else:

                awards_list = awards_block.find_elements_by_class_name('pv-accomplishments-block__summary-list-item')

                for award in awards_list:

                    dic_dic[i]['Awards'].append(award.text)

            # Publications
            try:

                publications_block = accomplishments_block.find_element_by_id('publications-expandable-content')

            except NoSuchElementException:

                dic_dic[i]['Publications'] = 'No publications data provided'

            else:

                publications_list = publications_block.find_elements_by_class_name('pv-accomplishments-block__summary-list-item')

                for publication in publications_list:

                    dic_dic[i]['Publications'].append(publication.text)


    df = pd.DataFrame.from_dict(dic_dic, orient='index')
    df.to_csv('output_file.csv')


# STEP 4: Clean up scraped data for text analysis in R

df = pd.read_csv('scrape_all.csv', header = 0, encoding = 'latin-1')
columns_to_be_expanded = list(set(df.columns) - {'URL', 'Title'})
columns_to_be_numeric = list(set(columns_to_be_expanded) - {'Position','Degree','Volunteer_Position','Company','Publications','Certifications','Awards','Volunteer_Organization','School'})
columns_to_be_expanded_strings = list(set(columns_to_be_expanded) - set(columns_to_be_numeric))

for c in columns_to_be_numeric:
    df[c] = df[c].str.strip('[]')
    e = '{}'.format(c)
    f = lambda x: e + '_{}'.format(x + 1)
    df = df.join(pd.DataFrame(df[c].str.split(',').values.tolist()).rename(columns = f).fillna(''))

#https://stackoverflow.com/questions/35491274/pandas-split-column-of-lists-into-multiple-columns
#https://stackoverflow.com/questions/44663903/pandas-split-column-of-lists-of-unequal-length-into-multiple-columns

for c in columns_to_be_expanded_strings:
    df[c] = df[c].str.strip('[]')
    df[c] = df[c].str.replace("',","%")
    e = '{}'.format(c)
    f = lambda x: e + '_{}'.format(x + 1)
    #df = df.join(pd.DataFrame(df[c].str.split(',').values.tolist()).rename(columns = f).fillna(''))
    df = df.join(pd.DataFrame(df[c].str.split("%").values.tolist()).rename(columns=f).fillna(''))

df2 = df.iloc[:, np.r_[0:2, 17:134]]

for c in df2.columns:
    df2[c] = df2[c].str.replace("'","")

#check to see if ncol jobs = companies = dates (same for volunteer and education)

comp_cols = [col for col in df2.columns if 'Company' in col]
job_cols = [col for col in df2.columns if 'Position' in col]
job_started_cols = [col for col in df2.columns if 'Job_started' in col]
job_ended_cols = [col for col in df2.columns if 'Job_ended' in col]

vol_org_cols = [col for col in df2.columns if 'Volunteer_Organization' in col]
vol_pos_cols = [col for col in df2.columns if 'Volunteer_Position' in col]
vol_started_cols = [col for col in df2.columns if 'Volunteer_started' in col]
vol_ended_cols = [col for col in df2.columns if 'Volunteer_ended' in col]

school_cols = [col for col in df2.columns if 'School' in col]
deg_cols = [col for col in df2.columns if 'Degree' in col]
deg_started_cols = [col for col in df2.columns if 'Degree_started' in col]
deg_ended_cols = [col for col in df2.columns if 'Degree_ended' in col]


comp_cols_nonzero = []
job_cols_nonzero = []
job_started_cols_nonzero = []
job_ended_cols_nonzero = []

vol_org_cols_nonzero = []
vol_pos_cols_nonzero = []
vol_started_cols_nonzero = []
vol_ended_cols_nonzero = []

school_cols_nonzero = []
deg_cols_nonzero = []
deg_started_cols_nonzero = []
deg_ended_cols_nonzero = []


for r in range(0,df2.shape[0]):
    comp_cols_nonzero.append(sum(x is not '' for x in list(df2.loc[r,comp_cols])))
    job_cols_nonzero.append(sum(x is not '' for x in list(df2.loc[r,job_cols])))
    job_started_cols_nonzero.append(sum(x is not '' for x in list(df2.loc[r,job_started_cols])))
    job_ended_cols_nonzero.append(sum(x is not '' for x in list(df2.loc[r, job_ended_cols])))

    vol_org_cols_nonzero.append(sum(x is not '' for x in list(df2.loc[r, vol_org_cols])))
    vol_pos_cols_nonzero.append(sum(x is not '' for x in list(df2.loc[r, vol_pos_cols])))
    vol_started_cols_nonzero.append(sum(x is not '' for x in list(df2.loc[r, vol_started_cols])))
    vol_ended_cols_nonzero.append(sum(x is not '' for x in list(df2.loc[r, vol_ended_cols])))

    school_cols_nonzero.append(sum(x is not '' for x in list(df2.loc[r, school_cols])))
    deg_cols_nonzero.append(sum(x is not '' for x in list(df2.loc[r, deg_cols])))
    deg_started_cols_nonzero.append(sum(x is not '' for x in list(df2.loc[r, deg_started_cols])))
    deg_ended_cols_nonzero.append(sum(x is not '' for x in list(df2.loc[r, deg_ended_cols])))

actual_job_cols_nonzero = list(map(operator.sub, job_cols_nonzero, vol_pos_cols_nonzero))
deg_cols_extra = list(map(operator.add, deg_ended_cols_nonzero, deg_started_cols_nonzero))
actual_deg_cols_nonzero = list(map(operator.sub, deg_cols_nonzero, deg_cols_extra))

job_check = []
edu_check = []
vol_check = []


for r in range(0,df2.shape[0]):

    if comp_cols_nonzero[r] == actual_job_cols_nonzero[r] == job_started_cols_nonzero[r] == job_ended_cols_nonzero[r]:
        job_check.append(True)
    else:
        job_check.append(False)


    if vol_org_cols_nonzero[r] == vol_pos_cols_nonzero[r] == vol_started_cols_nonzero[r] == vol_ended_cols_nonzero[r]:
        vol_check.append(True)
    else:
        vol_check.append(False)


    if school_cols_nonzero[r] == actual_deg_cols_nonzero[r] == deg_started_cols_nonzero[r] == deg_ended_cols_nonzero[r]:
        edu_check.append(True)
    else:
        edu_check.append(False)


sum(x is False for x in job_check)
okay = np.where(job_check)[0]
okay = list(okay)
full = list(range(0, 769))
bad_data = [x for x in full if x not in okay]

sum(x is False for x in edu_check)
okay = np.where(edu_check)[0]
okay = list(okay)
bad_data = [x for x in full if x not in okay]

sum(x is False for x in vol_check)
okay = np.where(vol_check)[0]
okay = list(okay)
bad_data = [x for x in full if x not in okay]
