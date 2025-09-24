CREATE DATABASE IF NOT EXISTS faculty_software;
USE faculty_software;

CREATE TABLE licenses (
    license_id INT AUTO_INCREMENT PRIMARY KEY,
    usage_terms VARCHAR(255) NOT NULL,
    usage_conditions TEXT NOT NULL
);

CREATE TABLE software (
    software_id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(255) NOT NULL UNIQUE,
    annotation TEXT,
    kind VARCHAR(100),
    version VARCHAR(50),
    distribution_location VARCHAR(255),
    license_id INT,
    FOREIGN KEY (license_id) REFERENCES licenses(license_id)
);

CREATE TABLE authors (
    author_id INT AUTO_INCREMENT PRIMARY KEY,
    full_name VARCHAR(255) NOT NULL,
    contact_info VARCHAR(255)
);

CREATE TABLE software_authors (
    software_id INT,
    author_id INT,
    PRIMARY KEY (software_id, author_id),
    FOREIGN KEY (software_id) REFERENCES software(software_id) ON DELETE CASCADE,
    FOREIGN KEY (author_id) REFERENCES authors(author_id) ON DELETE CASCADE
);

CREATE TABLE users (
    user_id INT AUTO_INCREMENT PRIMARY KEY,
    user_name VARCHAR(255) NOT NULL,
    faculty_department VARCHAR(255)
);

CREATE TABLE usage_statistics (
    stat_id INT AUTO_INCREMENT PRIMARY KEY,
    software_id INT,
    user_id INT,
    access_timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    usage_count INT DEFAULT 1,
    FOREIGN KEY (software_id) REFERENCES software(software_id) ON DELETE CASCADE,
    FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE
);
